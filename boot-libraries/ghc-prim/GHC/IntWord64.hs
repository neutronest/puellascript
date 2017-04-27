{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude, UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IntWord64
-- Copyright   :  (c) The University of Glasgow, 1997-2008
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Primitive operations on Int64# and Word64# on platforms where
-- WORD_SIZE_IN_BITS < 64.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module GHC.IntWord64 (
#if WORD_SIZE_IN_BITS < 64
        Int64#, Word64#, module GHC.IntWord64
#endif
    ) where

import GHC.Types () -- Make implicit dependency known to build system

#if WORD_SIZE_IN_BITS < 64

import GHC.Prim

eqWord64# :: Word64# -> Word64# -> Int#
eqWord64# = eqWord64#
neWord64# :: Word64# -> Word64# -> Int#
neWord64# = neWord64#
ltWord64# :: Word64# -> Word64# -> Int#
ltWord64# = ltWord64#
leWord64# :: Word64# -> Word64# -> Int#
leWord64# = leWord64#
gtWord64# :: Word64# -> Word64# -> Int#
gtWord64# = gtWord64#
geWord64# :: Word64# -> Word64# -> Int#
geWord64# = geWord64#
eqInt64# :: Int64# -> Int64# -> Int#
eqInt64# = eqInt64#
neInt64# :: Int64# -> Int64# -> Int#
neInt64# = neInt64#
ltInt64# :: Int64# -> Int64# -> Int#
ltInt64# = ltInt64#
leInt64# :: Int64# -> Int64# -> Int#
leInt64# = leInt64#
gtInt64# :: Int64# -> Int64# -> Int#
gtInt64# = gtInt64#
geInt64# :: Int64# -> Int64# -> Int#
geInt64# = geInt64#
quotInt64# :: Int64# -> Int64# -> Int64#
quotInt64# = quotInt64#
remInt64# :: Int64# -> Int64# -> Int64#
remInt64# = remInt64#
plusInt64# :: Int64# -> Int64# -> Int64#
plusInt64# = plusInt64#
minusInt64# :: Int64# -> Int64# -> Int64#
minusInt64# = minusInt64#
timesInt64# :: Int64# -> Int64# -> Int64#
timesInt64# = timesInt64#
negateInt64# :: Int64# -> Int64#
negateInt64# = negateInt64#
quotWord64# :: Word64# -> Word64# -> Word64#
quotWord64# = quotWord64#
remWord64# :: Word64# -> Word64# -> Word64#
remWord64# = remWord64#
and64# :: Word64# -> Word64# -> Word64#
and64# = and64#
or64# :: Word64# -> Word64# -> Word64#
or64# = or64#
xor64# :: Word64# -> Word64# -> Word64#
xor64# = xor64#
not64# :: Word64# -> Word64#
not64# = not64#
uncheckedShiftL64# :: Word64# -> Int# -> Word64#
uncheckedShiftL64# = uncheckedShiftL64#
uncheckedShiftRL64# :: Word64# -> Int# -> Word64#
uncheckedShiftRL64# = uncheckedShiftRL64#
uncheckedIShiftL64# :: Int64# -> Int# -> Int64#
uncheckedIShiftL64# = uncheckedIShiftL64#
uncheckedIShiftRA64# :: Int64# -> Int# -> Int64#
uncheckedIShiftRA64# = uncheckedIShiftRA64#
uncheckedIShiftRL64# :: Int64# -> Int# -> Int64#
uncheckedIShiftRL64# = uncheckedIShiftRL64#
int64ToWord64# :: Int64# -> Word64#
int64ToWord64# = int64ToWord64#
word64ToInt64# :: Word64# -> Int64#
word64ToInt64# = word64ToInt64#
intToInt64# :: Int# -> Int64#
intToInt64# = intToInt64#
int64ToInt# :: Int64# -> Int#
int64ToInt# = int64ToInt#
wordToWord64# :: Word# -> Word64#
wordToWord64# = wordToWord64#
word64ToWord# :: Word64# -> Word#
word64ToWord# = word64ToWord#

#endif

