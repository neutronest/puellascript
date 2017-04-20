{-# LANGUAGE TemplateHaskell #-}

module GHC.Unexported where

import Language.Haskell.TH
import GHC.Summon
import qualified StgSyn as GHC

showStgBinderInfo :: GHC.StgBinderInfo -> String
showStgBinderInfo x = case x of
    $(summon ''GHC.StgBinderInfo "NoStgBinderInfo" (\n -> ConP n [])) -> "NoStgBinderInfo"
    $(summon ''GHC.StgBinderInfo "SatCallsOnly" (\n -> ConP n [])) -> "SatCallsOnly"
