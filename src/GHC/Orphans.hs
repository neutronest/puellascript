{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.Orphans where

import qualified BasicTypes as GHC
import qualified ByteCodeTypes as GHC
import qualified CLabel as GHC
import qualified Cmm as GHC
import qualified Coercion as GHC
import qualified Compiler.Hoopl.Internals as Hoopl
import qualified CoreSyn as GHC
import qualified CostCentre as GHC
import qualified DataCon as GHC
import qualified DynFlags as GHC
import qualified ForeignCall as GHC
import qualified GHCi.RemoteTypes as GHC
import qualified HscTypes as GHC
import qualified Literal as GHC
import qualified Module as GHC
import qualified OccName as GHC
import qualified Outputable as GHC
import qualified PrimOp as GHC
import qualified SMRep as GHC
import qualified StgSyn as GHC
import qualified TyCoRep as GHC
import qualified TyCon as GHC
import qualified Var as GHC

instance Show GHC.CLabel where
    show = show . GHC.showSDocUnsafe . GHC.ppr

instance Show GHC.ModuleName where
    show = show . GHC.showSDocUnsafe . GHC.ppr

instance Show GHC.CmmType where
    show = show . GHC.showSDocUnsafe . GHC.ppr

instance Show GHC.StgHalfWord where
    show = show . GHC.showSDocUnsafe . GHC.ppr

instance Show GHC.Var where
    show = show . GHC.showSDocUnsafe . GHC.ppr

instance Show GHC.TyCon where
    show = show . GHC.showSDocUnsafe . GHC.ppr

instance Show GHC.Coercion where
    show = show . GHC.showSDocUnsafe . GHC.ppr

instance Show GHC.DataCon where
    show = show . GHC.showSDocUnsafe . GHC.ppr

instance Show GHC.CostCentreStack where
    show = show . GHC.showSDocUnsafe . GHC.ppr

instance Show GHC.StgBinderInfo where
    show x =
        if GHC.satCallsOnly x
            then "SatCallsOnly"
            else "NoStgBinderInfo"

instance Show GHC.SDoc where
    show = show . GHC.showSDocUnsafe

instance Show (GHC.ForeignRef a) where
    show = const "\"ForeignRef a\""

instance Show GHC.OccName where
    show = show . GHC.showSDocUnsafe . GHC.ppr

deriving instance Show GHC.Section

deriving instance
         (Show d, Show h, Show g) => Show (GHC.GenCmmDecl d h g)

deriving instance Show GHC.CmmLit

deriving instance Show GHC.CmmStatic

deriving instance Show GHC.CmmStatics

deriving instance Show GHC.CmmTickScope

deriving instance Show GHC.Module

deriving instance Show GHC.IsCafCC

deriving instance Show GHC.CostCentre

deriving instance Show id => Show (GHC.Tickish id)

deriving instance Show GHC.LocalReg

deriving instance Show GHC.CmmReg

deriving instance Show GHC.Area

deriving instance Show GHC.CmmExpr

deriving instance Show GHC.CCallConv

deriving instance Show GHC.ForeignHint

deriving instance Show GHC.CmmReturnInfo

deriving instance Show GHC.ForeignConvention

deriving instance Show GHC.ForeignTarget

deriving instance (Show e, Show x) => Show (GHC.CmmNode e x)

deriving instance Show Hoopl.C

deriving instance Show Hoopl.O

deriving instance
         (Show (n Hoopl.C Hoopl.O), Show (n Hoopl.O Hoopl.O),
          Show (n Hoopl.O Hoopl.C), Show (n Hoopl.C Hoopl.C)) =>
         Show (Hoopl.Block n e x)

deriving instance Show t => Show (Hoopl.MaybeO ex t)

deriving instance
         (Show (block n Hoopl.C Hoopl.O), Show (block n Hoopl.O Hoopl.O),
          Show (block n Hoopl.O Hoopl.C), Show (block n Hoopl.C Hoopl.C)) =>
         Show (Hoopl.Graph' block n e x)

deriving instance
         (Show (n Hoopl.C Hoopl.O), Show (n Hoopl.O Hoopl.O),
          Show (n Hoopl.O Hoopl.C), Show (n Hoopl.C Hoopl.C)) =>
         Show (GHC.GenCmmGraph n)

deriving instance Show GHC.ArgDescr

deriving instance Show GHC.ClosureTypeInfo

deriving instance Show GHC.SMRep

deriving instance Show GHC.ProfilingInfo

deriving instance Show GHC.C_SRT

deriving instance Show GHC.CmmInfoTable

deriving instance Show GHC.CmmStackInfo

deriving instance Show GHC.CmmTopInfo

deriving instance Show GHC.FunctionOrData

deriving instance
         (Show tyvar, Show argf) => Show (GHC.TyVarBndr tyvar argf)

deriving instance Show GHC.ArgFlag

deriving instance Show GHC.TyLit

deriving instance Show GHC.Type

deriving instance Show GHC.Literal

deriving instance Show occ => Show (GHC.GenStgArg occ)

deriving instance Show GHC.PrimOpVecCat

deriving instance Show GHC.PrimOp

deriving instance Show GHC.PrimCall

deriving instance Show GHC.CCallTarget

deriving instance Show GHC.CCallSpec

deriving instance Show GHC.ForeignCall

deriving instance Show GHC.StgOp

deriving instance Show GHC.AltType

deriving instance Show GHC.AltCon

deriving instance
         (Show bndr, Show occ) => Show (GHC.GenStgExpr bndr occ)

deriving instance Show GHC.UpdateFlag

deriving instance
         (Show bndr, Show occ) => Show (GHC.GenStgRhs bndr occ)

deriving instance
         (Show bndr, Show occ) => Show (GHC.GenStgBinding bndr occ)

deriving instance
         (Show bndr, Show occ) => Show (GHC.GenStgTopBinding bndr occ)

deriving instance Show b => Show (GHC.Expr b)

deriving instance Show b => Show (GHC.Bind b)

deriving instance Show GHC.ForeignStubs

deriving instance Show GHC.InstalledUnitId

deriving instance Show GHC.HpcInfo

deriving instance Show GHC.CgBreakInfo

deriving instance Show GHC.ModBreaks

deriving instance Show GHC.SptEntry

deriving instance Show GHC.CgGuts

deriving instance Show GHC.PkgConfRef

deriving instance Show GHC.PackageDBFlag
