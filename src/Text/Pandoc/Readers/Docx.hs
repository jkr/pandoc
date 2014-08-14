{-# LANGUAGE PatternGuards, OverloadedStrings #-}

{-
Copyright (C) 2014 Jesse Rosenthal <jrosenthal@jhu.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.Docx
   Copyright   : Copyright (C) 2014 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Conversion of Docx type (defined in Text.Pandoc.Readers.Docx.Parse)
to 'Pandoc' document.  -}

{-
Current state of implementation of Docx entities ([x] means
implemented, [-] means partially implemented):

* Blocks

  - [X] Para
  - [X] CodeBlock (styled with `SourceCode`)
  - [X] BlockQuote (styled with `Quote`, `BlockQuote`, or, optionally,
        indented)
  - [X] OrderedList
  - [X] BulletList
  - [X] DefinitionList (styled with adjacent `DefinitionTerm` and `Definition`)
  - [X] Header (styled with `Heading#`)
  - [ ] HorizontalRule
  - [-] Table (column widths and alignments not yet implemented)

* Inlines

  - [X] Str
  - [X] Emph (From italics. `underline` currently read as span. In
        future, it might optionally be emph as well)
  - [X] Strong
  - [X] Strikeout
  - [X] Superscript
  - [X] Subscript
  - [X] SmallCaps
  - [ ] Quoted
  - [ ] Cite
  - [X] Code (styled with `VerbatimChar`)
  - [X] Space
  - [X] LineBreak (these are invisible in Word: entered with Shift-Return)
  - [ ] Math
  - [X] Link (links to an arbitrary bookmark create a span with the target as
        id and "anchor" class)
  - [-] Image (Links to path in archive. Future option for
        data-encoded URI likely.)
  - [X] Note (Footnotes and Endnotes are silently combined.)
-}

module Text.Pandoc.Readers.Docx
       ( readDocx
       ) where

import Codec.Archive.Zip
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Builder
import Text.Pandoc.Walk
import Text.Pandoc.Readers.Docx.Parse
import Text.Pandoc.Readers.Docx.Lists
-- import Text.Pandoc.Readers.Docx.Reducible
import Text.Pandoc.Readers.Docx.Reducible2
import Text.Pandoc.Shared
import Text.Pandoc.MediaBag (insertMedia, MediaBag)
import Data.Maybe (mapMaybe, isJust, fromJust)
import Data.List (delete, stripPrefix, (\\), intersect)
import Data.Monoid
import Text.TeXMath (writeTeX)
import Data.Default (Default)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Sequence (ViewR(..), ViewL(..), viewl, viewr)
import qualified Data.Sequence as Seq (null)

readDocx :: ReaderOptions
         -> B.ByteString
         -> (Pandoc, MediaBag)
readDocx opts bytes =
  case archiveToDocx (toArchive bytes) of
    Right docx -> (Pandoc meta blks, mediaBag) where
      (meta, blks, mediaBag) = (docxToOutput opts docx)
    Left _   -> error $ "couldn't parse docx file"

data DState = DState { docxAnchorMap :: M.Map String String
                     , docxMediaBag      :: MediaBag
                     , docxDropCap       :: RInlines
                     }

instance Default DState where
  def = DState { docxAnchorMap = M.empty
               , docxMediaBag  = mempty
               , docxDropCap   = mempty
               }

data DEnv = DEnv { docxOptions  :: ReaderOptions
                 , docxInHeaderBlock :: Bool }

instance Default DEnv where
  def = DEnv def False

type DocxContext = ReaderT DEnv (State DState)

evalDocxContext :: DocxContext a -> DEnv -> DState -> a
evalDocxContext ctx env st = evalState (runReaderT ctx env) st

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

-- This is empty, but we put it in for future-proofing.
spansToKeep :: [String]
spansToKeep = []

divsToKeep :: [String]
divsToKeep = ["list-item", "Definition", "DefinitionTerm"]

metaStyles :: M.Map String String
metaStyles = M.fromList [ ("Title", "title")
                        , ("Subtitle", "subtitle")
                        , ("Author", "author")
                        , ("Date", "date")
                        , ("Abstract", "abstract")]

sepBodyParts :: [BodyPart] -> ([BodyPart], [BodyPart])
sepBodyParts = span (\bp -> (isMetaPar bp || isEmptyPar bp))

isMetaPar :: BodyPart -> Bool
isMetaPar (Paragraph pPr _) =
  not $ null $ intersect (pStyle pPr) (M.keys metaStyles)
isMetaPar _ = False

isEmptyPar :: BodyPart -> Bool
isEmptyPar (Paragraph _ parParts) =
  all isEmptyParPart parParts
  where
    isEmptyParPart (PlainRun (Run _ runElems)) = all isEmptyElem runElems
    isEmptyParPart _ = False
    isEmptyElem (TextRun s) = trim s == ""
    isEmptyElem _           = True
isEmptyPar _ = False

bodyPartsToMeta' :: [BodyPart] -> DocxContext (M.Map String MetaValue)
bodyPartsToMeta' [] = return M.empty
bodyPartsToMeta' (bp : bps)
  | (Paragraph pPr parParts) <- bp
  , (c : _)<- intersect (pStyle pPr) (M.keys metaStyles)
  , (Just metaField) <- M.lookup c metaStyles = do
    inlines <- parPartsToInlines parParts
    remaining <- bodyPartsToMeta' bps
    let
      f (MetaInlines ils) (MetaInlines ils') = MetaBlocks [Para ils, Para ils']
      f (MetaInlines ils) (MetaBlocks blks) = MetaBlocks ((Para ils) : blks)
      f m (MetaList mv) = MetaList (m : mv)
      f m n             = MetaList [m, n]
    return $ M.insertWith f metaField (MetaInlines (toList $ unReduce inlines)) remaining
bodyPartsToMeta' (_ : bps) = bodyPartsToMeta' bps

bodyPartsToMeta :: [BodyPart] -> DocxContext Meta
bodyPartsToMeta bps = do
  mp <- bodyPartsToMeta' bps
  let mp' =
        case M.lookup "author" mp of
          Just mv -> M.insert "author" (fixAuthors mv) mp
          Nothing -> mp
  return $ Meta mp'

fixAuthors :: MetaValue -> MetaValue
fixAuthors (MetaBlocks blks) =
  MetaList $ map g $ filter f blks
    where f (Para _) = True
          f _          = False
          g (Para ils) = MetaInlines ils
          g _          = MetaInlines []
fixAuthors mv = mv

runStyleToContainers :: RunStyle -> [(RInlines -> RInlines)]
runStyleToContainers rPr =
  let spanClassToContainers :: String -> [(RInlines -> RInlines)]
      spanClassToContainers s | s `elem` codeSpans =
        [\rils -> code <$> (stringify <$> rils)]
      spanClassToContainers s | s `elem` spansToKeep =
        [\rils -> spanWith ("", [s], []) <$> rils]
      spanClassToContainers  _     = []

      classContainers = case rStyle rPr of
        Nothing -> []
        Just s  -> spanClassToContainers s

      resolveFmt :: Bool -> Maybe Bool -> Bool
      resolveFmt _ (Just True) = True
      resolveFmt _ (Just False) = False
      resolveFmt bool Nothing   = bool

      formatters = mapMaybe
                   (\f -> if (isJust f)
                          then Just (\ril -> (fromJust f) <$> ril)
                          else Nothing)
                   [ if resolveFmt
                        (rStyle rPr `elem` [Just "Strong", Just "Bold"])
                        (isBold rPr)
                     then (Just strong)
                     else Nothing
                   , if resolveFmt
                        (rStyle rPr `elem` [Just"Emphasis", Just "Italic"])
                        (isItalic rPr)
                     then (Just emph)
                     else Nothing
                   , if resolveFmt False (isSmallCaps rPr)
                     then (Just smallcaps)
                     else Nothing
                   , if resolveFmt False (isStrike rPr)
                     then (Just strikeout)
                     else Nothing
                   , if isSuperScript rPr then (Just superscript) else Nothing
                   , if isSubScript rPr then (Just subscript) else Nothing
                   , rUnderline rPr >>=
                     (\f -> if f == "single" then (Just emph) else Nothing)
                 ]
  in
   classContainers ++ formatters

parStyleToContainers :: ParagraphStyle -> [(RBlocks -> RBlocks)]
parStyleToContainers pPr | (c:cs) <- pStyle pPr, Just n <- isHeaderClass c =
  let attr = ("", delete ("Heading" ++ show n) cs, [])
  in
   [\_ -> headerWith attr n <$> mempty]
parStyleToContainers pPr | (c:cs) <- pStyle pPr, c `elem` divsToKeep =
  let pPr' = pPr { pStyle = cs }
  in
   (divWith ("", [c], []) <$>) : (parStyleToContainers pPr')
parStyleToContainers pPr | (c:cs) <- pStyle pPr, c `elem` codeDivs =
  -- This is a bit of a cludge. We make the codeblock from the raw
  -- parparts in bodyPartToBlocks. But we need something to match against.
  let pPr' = pPr { pStyle = cs }
  in
   (\_ -> red $ codeBlock "") : (parStyleToContainers pPr')
parStyleToContainers pPr | (c:cs) <- pStyle pPr,  c `elem` listParagraphDivs =
  let pPr' = pPr { pStyle = cs, indentation = Nothing}
  in
   (divWith ("", [c], []) <$>) : (parStyleToContainers pPr')
parStyleToContainers pPr | (c:cs) <- pStyle pPr, c `elem` blockQuoteDivs =
  let pPr' = pPr { pStyle = cs \\ blockQuoteDivs }
  in
   (blockQuote <$>) : (parStyleToContainers pPr')
parStyleToContainers pPr | (_:cs) <- pStyle pPr =
  let pPr' = pPr { pStyle = cs}
  in
    parStyleToContainers pPr'
parStyleToContainers pPr | null (pStyle pPr),
                          Just left <- indentation pPr >>= leftParIndent,
                          Just hang <- indentation pPr >>= hangingParIndent =
  let pPr' = pPr { indentation = Nothing }
  in
   case (left - hang) > 0 of
     True -> (blockQuote <$>) : (parStyleToContainers pPr')
     False -> parStyleToContainers pPr'
parStyleToContainers pPr | null (pStyle pPr),
                          Just left <- indentation pPr >>= leftParIndent =
  let pPr' = pPr { indentation = Nothing }
  in
   case left > 0 of
     True -> (blockQuote <$>): (parStyleToContainers pPr')
     False -> parStyleToContainers pPr'
parStyleToContainers _ = []

-- strToInlines :: String -> [Inline]
-- strToInlines = toList . text

codeSpans :: [String]
codeSpans = ["VerbatimChar"]

blockQuoteDivs :: [String]
blockQuoteDivs = ["Quote", "BlockQuote", "BlockQuotation"]

codeDivs :: [String]
codeDivs = ["SourceCode"]

runElemToInlines :: RunElem -> RInlines
runElemToInlines (TextRun s) = red $ text s
runElemToInlines (LnBrk) = red linebreak
runElemToInlines (Tab) = red space

runElemToString :: RunElem -> String
runElemToString (TextRun s) = s
runElemToString (LnBrk) = ['\n']
runElemToString (Tab) = ['\t']

runElemsToString :: [RunElem] -> String
runElemsToString = concatMap runElemToString

runToString :: Run -> String
runToString (Run _ runElems) = runElemsToString runElems
runToString _ = ""

parPartToString :: ParPart -> String
parPartToString (PlainRun run) = runToString run
parPartToString (InternalHyperLink _ runs) = concatMap runToString runs
parPartToString (ExternalHyperLink _ runs) = concatMap runToString runs
parPartToString _ = ""

inlineCodeContainer :: (RInlines -> RInlines) -> Bool
inlineCodeContainer f = case viewl $ unMany $ unReduce $ f mempty of
  ((Code _ _) :< _) -> True
  _         -> False

restack :: [(a -> a)] -> a -> a
restack [] x = x
restack (f:fs) x = f $ restack fs x

runToInlines :: Run -> DocxContext RInlines
runToInlines (Run rs runElems)
  | any inlineCodeContainer (runStyleToContainers rs) =
      return $
      restack (runStyleToContainers rs) $ (red . str) $ runElemsToString runElems
  | otherwise =
      return $
      restack (runStyleToContainers rs) (mconcat $ map runElemToInlines runElems)
runToInlines (Footnote bps) = do
  blksList <- mapM bodyPartToBlocks bps
  return $ note <$> mconcat blksList
runToInlines (Endnote bps) = do
  blksList <- mapM bodyPartToBlocks bps
  return $ note <$> mconcat blksList
runToInlines (InlineDrawing fp bs) = do
  mediaBag <- gets docxMediaBag
  modify $ \s -> s { docxMediaBag = insertMedia fp Nothing bs mediaBag }
  return $ red $ image fp "" ""


parPartToInlines :: ParPart -> DocxContext RInlines
parPartToInlines (PlainRun r) = runToInlines r
parPartToInlines (Insertion _ author date runs) = do
  opts <- asks docxOptions
  case readerTrackChanges opts of
    AcceptChanges -> mconcat <$> mapM runToInlines runs
    RejectChanges -> return mempty
    AllChanges    -> do
      ils <- mconcat <$> mapM runToInlines runs
      let attr = ("", ["insertion"], [("author", author), ("date", date)])
      return $ spanWith attr <$> ils
parPartToInlines (Deletion _ author date runs) = do
  opts <- asks docxOptions
  case readerTrackChanges opts of
    AcceptChanges -> return mempty
    RejectChanges -> mconcat <$> mapM runToInlines runs
    AllChanges    -> do
      ils <- mconcat <$> mapM runToInlines runs
      let attr = ("", ["deletion"], [("author", author), ("date", date)])
      return $ spanWith attr <$> ils
parPartToInlines (BookMark _ anchor) | anchor `elem` dummyAnchors = return mempty
parPartToInlines (BookMark _ anchor) =
  -- We record these, so we can make sure not to overwrite
  -- user-defined anchor links with header auto ids.
  do
    -- get whether we're in a header.
    inHdrBool <- asks docxInHeaderBlock
    -- Get the anchor map.
    anchorMap <- gets docxAnchorMap
    -- We don't want to rewrite if we're in a header, since we'll take
    -- care of that later, when we make the header anchor. If the
    -- bookmark were already in uniqueIdent form, this would lead to a
    -- duplication. Otherwise, we check to see if the id is already in
    -- there. Rewrite if necessary. This will have the possible effect
    -- of rewriting user-defined anchor links. However, since these
    -- are not defined in pandoc, it seems like a necessary evil to
    -- avoid an extra pass.
    let newAnchor =
          if not inHdrBool && anchor `elem` (M.elems anchorMap)
          then uniqueIdent [Str anchor] (M.elems anchorMap)
          else anchor
    unless inHdrBool
      (modify $ \s -> s { docxAnchorMap = M.insert anchor newAnchor anchorMap})
    return $ spanWith (newAnchor, ["anchor"], []) <$> mempty
parPartToInlines (Drawing fp bs) = do
  mediaBag <- gets docxMediaBag
  modify $ \s -> s { docxMediaBag = insertMedia fp Nothing bs mediaBag }
  return $ red $ image fp "" ""
parPartToInlines (InternalHyperLink anchor runs) = do
  ils <- mconcat <$> mapM runToInlines runs
  return $ link ('#' : anchor) "" <$> ils
parPartToInlines (ExternalHyperLink target runs) = do
  ils <- mconcat <$> mapM runToInlines runs
  return $ link target "" <$> ils
parPartToInlines (PlainOMath exps) = do
  return $ red $ math $ writeTeX exps


isAnchorSpan :: Inline -> Bool
isAnchorSpan (Span (_, classes, kvs) ils) =
  classes == ["anchor"] &&
  null kvs &&
  null ils
isAnchorSpan _ = False

dummyAnchors :: [String]
dummyAnchors = ["_GoBack"]

makeHeaderAnchor :: Block -> DocxContext Block
-- If there is an anchor already there (an anchor span in the header,
-- to be exact), we rename and associate the new id with the old one.
makeHeaderAnchor (Header n (_, classes, kvs) ils)
  | xs <- filter isAnchorSpan ils
  , idents <- filter (\i -> notElem i dummyAnchors) $
              map (\(Span (ident, _, _) _) -> ident) xs
  , not $ null idents =
    do
      hdrIDMap <- gets docxAnchorMap
      let newIdent = uniqueIdent ils (M.elems hdrIDMap)
          newMap   = M.fromList $ map (\i -> (i, newIdent)) idents
      modify $ \s -> s {docxAnchorMap = M.union newMap hdrIDMap}
      return $ Header n (newIdent, classes, kvs) (ils \\ xs)
-- Otherwise we just give it a name, and register that name (associate
-- it with itself.)
makeHeaderAnchor (Header n (_, classes, kvs) ils) =
  do
    hdrIDMap <- gets docxAnchorMap
    let newIdent = uniqueIdent ils (M.elems hdrIDMap)
    modify $ \s -> s {docxAnchorMap = M.insert newIdent newIdent hdrIDMap}
    return $ Header n (newIdent, classes, kvs) ils
makeHeaderAnchor blk = return blk

parPartsToInlines :: [ParPart] -> DocxContext RInlines
parPartsToInlines parparts = mconcat <$> mapM parPartToInlines parparts

cellToBlocks :: Cell -> DocxContext RBlocks
cellToBlocks (Cell bps) = mconcat <$> mapM bodyPartToBlocks bps

rowToBlocksList :: Row -> DocxContext [RBlocks]
rowToBlocksList (Row cells) = mapM cellToBlocks cells

isBlockCodeContainer :: (RBlocks -> RBlocks) -> Bool
isBlockCodeContainer f = case viewl $ unMany $ unReduce (f mempty) of
  (CodeBlock _ _ :< _) -> True
  _                   -> False

isHeaderContainer :: (RBlocks -> RBlocks) -> Bool
isHeaderContainer f = case viewl $ unMany $ unReduce (f mempty) of
  (Header _ _ _ :< _) -> True
  _                  -> False

trimLineBreaks :: [Inline] -> [Inline]
trimLineBreaks [] = []
trimLineBreaks (LineBreak : ils) = trimLineBreaks ils
trimLineBreaks ils
  | (LineBreak : ils') <- reverse ils = trimLineBreaks (reverse ils')
trimLineBreaks ils = ils

bodyPartToBlocks :: BodyPart -> DocxContext RBlocks
bodyPartToBlocks (Paragraph pPr parparts)
  | any isBlockCodeContainer (parStyleToContainers pPr) =
    let
      otherConts = filter (not . isBlockCodeContainer) (parStyleToContainers pPr)
    in
     return $
     restack otherConts $ (red . codeBlock) (concatMap parPartToString parparts)
bodyPartToBlocks (Paragraph pPr parparts)
  | any isHeaderContainer (parStyleToContainers pPr) = do
    ils <- -- (trimLineBreaks . normalizeSpaces) <$>
           local (\s -> s{docxInHeaderBlock = True})
           (parPartsToInlines parparts)
    let hdrFun = head $ filter isHeaderContainer (parStyleToContainers pPr)
        (Header n attr _ :< _) = viewl $ unMany $ unReduce $ hdrFun mempty
    let hdr = headerWith attr n <$> ils
        hdr'= makeHeaderAnchor <$> unReduce hdr
    hs <- sequence $ toList hdr'
    return $ red $ fromList hs
bodyPartToBlocks (Paragraph pPr parparts) = do
  ils <- parPartsToInlines parparts
  dropIls <- gets docxDropCap
  let ils' = dropIls <> ils
  if dropCap pPr
    then do modify $ \s -> s { docxDropCap = ils' }
            return mempty
    else do modify $ \s -> s { docxDropCap = mempty }
            return $ case isNull $ unReduce ils' of
              True -> mempty
              _ -> restack (parStyleToContainers pPr) $ para <$> ils'
bodyPartToBlocks (ListItem pPr numId lvl levelInfo parparts) = do
  let
    kvs = case levelInfo of
      (_, fmt, txt, Just start) -> [ ("level", lvl)
                                   , ("num-id", numId)
                                   , ("format", fmt)
                                   , ("text", txt)
                                   , ("start", (show start))
                                   ]

      (_, fmt, txt, Nothing)    -> [ ("level", lvl)
                                   , ("num-id", numId)
                                   , ("format", fmt)
                                   , ("text", txt)
                                   ]
  blks <- bodyPartToBlocks (Paragraph pPr parparts)
  return $ divWith ("", ["list-item"], kvs) <$> blks
bodyPartToBlocks (Tbl _ _ _ []) =
  return $ para <$> mempty
bodyPartToBlocks (Tbl cap _ look (r:rs)) = do
  let caption = text cap
      (hdr, rows) = case firstRowFormatting look of
        True -> (Just r, rs)
        False -> (Nothing, r:rs)
  hdrCells <- case hdr of
    Just r' -> rowToBlocksList r'
    Nothing -> return []

  cells <- mapM rowToBlocksList rows

  let size = case null hdrCells of
        True -> length $ head cells
        False -> length $ hdrCells
      --
      -- The two following variables (horizontal column alignment and
      -- relative column widths) go to the default at the
      -- moment. Width information is in the TblGrid field of the Tbl,
      -- so should be possible. Alignment might be more difficult,
      -- since there doesn't seem to be a column entity in docx.
      alignments = replicate size AlignDefault
      widths = replicate size 0 :: [Double]

  return $ red $ table caption (zip alignments widths)
    (map unReduce hdrCells)
    (map (\r -> map unReduce r) cells)
bodyPartToBlocks (OMathPara e) = do
  return $ para <$> (red . displayMath) (writeTeX e)


-- replace targets with generated anchors.
rewriteLink :: Inline -> DocxContext Inline
rewriteLink l@(Link ils ('#':target, title)) = do
  anchorMap <- gets docxAnchorMap
  return $ case M.lookup target anchorMap of
    Just newTarget -> (Link ils ('#':newTarget, title))
    Nothing        -> l
rewriteLink il = return il

bodyToOutput :: Body -> DocxContext (Meta, [Block], MediaBag)
bodyToOutput (Body bps) = do
  let (metabps, blkbps) = sepBodyParts bps
  meta <- bodyPartsToMeta metabps
  blks <- mconcat <$> mapM bodyPartToBlocks blkbps
  blks' <- walkM rewriteLink $ unReduce blks
  mediaBag <- gets docxMediaBag
  return $ (meta,
            blocksToDefinitions $ blocksToBullets $ toList blks',
            mediaBag)

docxToOutput :: ReaderOptions -> Docx -> (Meta, [Block], MediaBag)
docxToOutput opts (Docx (Document _ body)) =
  let dEnv   = def { docxOptions  = opts} in
   evalDocxContext (bodyToOutput body) dEnv def

ilToCode :: Inline -> String
ilToCode (Str s) = s
ilToCode Space = " "
ilToCode _     = ""

isHeaderClass :: String -> Maybe Int
isHeaderClass s | Just s' <- stripPrefix "Heading" s =
  case reads s' :: [(Int, String)] of
    [] -> Nothing
    ((n, "") : []) -> Just n
    _       -> Nothing
isHeaderClass _ = Nothing
