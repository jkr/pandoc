{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{- |
   Module      : Text.Pandoc.Writers.Powerpoint.Animation
   Copyright   : Copyright (C) 2019 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Creation of PresentationML animation elements for incremental
lists. This module makes no attempt to be a complete implementation of
PresentationML animation. It just tries to create enough to produce
pandoc presentation incremental lists.
-}

module Text.Pandoc.Writers.Powerpoint.Animation (
                                                ) where

import Prelude
import Text.Pandoc.Writers.OOXML (mknode)
import Control.Monad.State
import Text.XML.Light

data AnimState = AnimState { animStTnId :: Int }
  deriving (Show, Eq)

createInnerPar :: Int -> Int -> Int -> State AnimState Element
createInnerPar spId pRgSt pRgEnd = do
  curTnId <- gets animStTnId
  let output =
        mknode "p:par" []
        [ mknode "p:cTn" [ ("id", show curTnId)
                           , ("presetId", "1")
                           , ("presetClass", "entr")
                           , ("presetSubtype", "0")
                           , ("fill", "hold")
                           , ("grpId", "0")
                           , ("nodeType", "clickEffect")
                           ]
          [ mknode "p:stCondLst" []
            [ mknode "p:cond" [("delay", "0")] ()
            ]
          , mknode "p:childTnLst" []
            [ mknode "p:set" []
              [ mknode "p:cBhvr" []
                [ mknode "p:cTn" [ ("id", show $ curTnId + 1)
                                 , ("fill", "hold")
                                 ]
                  [ mknode "p:stCondLst" []
                    [ mknode "p:cond" [("delay", "0")] ()
                    ]
                  ]
                , mknode "p:tgtEl" []
                  [ mknode "p:spTgt" [("spid", show spId)]
                    [ mknode "p:txEl" []
                      [ mknode "p:pRg" [ ("st", show $ pRgSt)
                                       , ("end", show $ pRgEnd)
                                       ] ()
                      ]
                    ]
                  ]
                , mknode "p:attrNameLst" []
                  [ mknode "p:attrName" [] "style.visibility"
                  ]
                ]
              , mknode "p:to" []
                [ mknode "p:strVal" [("val", "visible")] ()
                ]
              ]
            ]
          ]
        ]
  modify $ \st -> st {animStTnId = curTnId + 2 }
  return $ output

createTiming :: [Element] -> Element
createTiming innerParElements =
  mknode "p:timing" []
  [ mknode "p:tnLst" []
    [ mknode "p:par" []
      [ mknode "p:cTn" [ ("id", "1")
                       , ("dur", "indefinite")
                       , ("restart", "never")
                       , ("nodeTYpe", "tmRoot")
                       ]
        [ mknode "p:childTnLst" []
          [ mknode "p:seq" []
            [ mknode "p:cTn" [ ("id", "2")
                             , ("dur", "indefinite")
                             , ("nodeType", "mainSeq")
                             ]
              innerParElements
            ]
          ]
        ]
      ]
    ]
  ]
