module GHC.Summon where

import Data.List
import Language.Haskell.TH

summon :: Name -> String -> (Name -> Pat) -> Q Pat
summon t c mp = do
    info <- reify t
    case info of
        TyConI dec -> do
            let dec2cons dec' =
                    case dec' of
                        DataD _ _ _ _ cons _ -> cons
                        NewtypeD _ _ _ _ con _ -> [con]
                        _ ->
                            error $
                            show dec' ++
                            " is not a data or newtype declaration."
                con2name con =
                    case con of
                        NormalC name _ -> name
                        RecC name _ -> name
                        InfixC _ name _ -> name
                        ForallC _ _ con' -> con2name con'
                        _ -> error $ show con ++ " is unsupported by con2name."
                cnames = map con2name $ dec2cons dec
            case find ((== c) . nameBase) cnames of
                Just n -> pure $ mp n
                _ ->
                    fail $
                    c ++ " is not among the constructors of " ++ nameBase t
        _ -> fail $ nameBase t ++ " is not a plain type constructor."
