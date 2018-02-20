{-# LANGUAGE PatternGuards #-}

module Text.Pandoc.Writers.Powerpoint.Output.Layout (
                                                    ) where


import Text.Pandoc.Writers.OOXML
import Text.XML.Light
import qualified Text.XML.Light.Cursor as XMLC
import Data.Maybe (mapMaybe, listToMaybe)


data PlaceHolder = PlaceHolder { phType :: Maybe String
                               , phIdx :: Maybe Int
                               } deriving (Show)


data LayoutShape = LayoutShape { layoutShapeXML :: Element
                               , layoutShapePlaceHolder :: PlaceHolder
                               } deriving (Show)

data Layout = Layout { layoutName :: String
                     , layoutWrapper :: Element
                     , layoutShapes :: [LayoutShape]
                     } deriving (Show)

withPhIdx' :: [LayoutShape] -> Int -> Maybe LayoutShape
withPhIdx' [] _ = Nothing
withPhIdx' (sp : sps) n =
  case phIdx (layoutShapePlaceHolder sp) of
    Just n' | n' == n -> Just sp
    _                 -> sps `withPhIdx'` n

withPhIdx :: Layout -> Int -> Maybe LayoutShape
withPhIdx = withPhIdx' . layoutShapes

withPhType' :: [LayoutShape] -> String -> Maybe LayoutShape
withPhType' [] _ = Nothing
withPhType' (sp : sps) tp =
  case phType (layoutShapePlaceHolder sp) of
    Just tp' | tp' == tp -> Just sp
    _                 -> sps `withPhType'` tp

withPhType :: Layout -> String -> Maybe LayoutShape
withPhType = withPhType' . layoutShapes


toLayoutShape :: NameSpaces -> Element -> Maybe LayoutShape
toLayoutShape ns element
  | isElem ns "p" "sp" element
  , Just nvSpPr <- findChild (elemName ns "p" "nvSpPr") element
  , Just nvPr <- findChild (elemName ns "p" "nvPr") nvSpPr
  , Just ph <- findChild (elemName ns "p" "ph") nvPr =
      let phType' = findAttr (QName "type" Nothing Nothing) ph
          phIdx' = findAttr (QName "idx" Nothing Nothing) ph >>=
                   (listToMaybe . reads) >>=
                   (return . fst)
          placeHolder = PlaceHolder { phType = phType'
                                    , phIdx = phIdx'
                                    }
      in
        Just $ LayoutShape { layoutShapeXML = element
                           , layoutShapePlaceHolder = placeHolder
                           }
toLayoutShape _ _ = Nothing

toLayout :: Element -> Maybe Layout
toLayout element
  | ns <- elemToNameSpaces element
  , isElem ns "p" "sldLayout" element
  , Just cSld <- findChild (elemName ns "p" "cSld") element
  , Just layoutName' <- findAttr (QName "name" Nothing Nothing) cSld
  , Just spTree <- findChild (elemName ns "p" "spTree") cSld
  , Just wrapper <- removeShapes ns cSld =
      Just $ Layout { layoutName = layoutName'
                    , layoutWrapper = wrapper
                    , layoutShapes = mapMaybe (toLayoutShape ns) $
                                     elChildren spTree
                    }
toLayout _ = Nothing

removeShapesFromChildren :: NameSpaces -> [Content] -> [Content]
removeShapesFromChildren ns cs =
  concatMap f cs where f (Elem e) | isElem ns "p" "sp" e = []
                       f ct = [ct]

removeShapes' :: NameSpaces -> XMLC.Cursor -> XMLC.Cursor
removeShapes' ns cur =
  case XMLC.current cur of
    (Elem e) | isElem ns "p" "spTree" e ->
      let cont' = removeShapesFromChildren ns $ elContent e
      in
        XMLC.setContent (Elem e{elContent = cont'}) cur
    _ -> case XMLC.nextDF cur of
           Just cur' -> removeShapes' ns cur'
           Nothing   -> cur

removeShapes :: NameSpaces -> Element -> Maybe Element
removeShapes ns element =
  case XMLC.toTree $ XMLC.root $ removeShapes' ns $ XMLC.fromElement element of
    Elem element' -> Just element'
    _             -> Nothing
