{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

#include "MachDeps.h"

-- |
-- Module      :  GHC.Integer.GMP.Internals
-- Copyright   :  (c) Herbert Valerio Riedel 2014
-- License     :  BSD3
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (GHC Extensions)
--
-- This modules provides access to the 'Integer' constructors and
-- exposes some highly optimized GMP-operations.
--
-- Note that since @integer-gmp@ does not depend on `base`, error
-- reporting via exceptions, 'error', or 'undefined' is not
-- available. Instead, the low-level functions will crash the runtime
-- if called with invalid arguments.
--
-- See also
-- <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/Integer GHC Commentary: Libraries/Integer>.

module GHC.Integer.GMP.Internals
    ( -- * The 'Integer' type
      Integer(..)
    , isValidInteger#

      -- ** Basic 'Integer' operations

    , module GHC.Integer

      -- ** Additional 'Integer' operations
    , bitInteger
    , popCountInteger
    , gcdInteger
    , gcdExtInteger
    , lcmInteger
    , sqrInteger
    , powModInteger
    , recipModInteger

      -- ** Additional conversion operations to 'Integer'
    , wordToNegInteger
    , bigNatToInteger
    , bigNatToNegInteger

      -- * The 'BigNat' type
    , BigNat(..)

    , GmpLimb, GmpLimb#
    , GmpSize, GmpSize#

      -- **

    , isValidBigNat#
    , sizeofBigNat#
    , zeroBigNat
    , oneBigNat
    , nullBigNat

      -- ** Conversions to/from 'BigNat'

    , byteArrayToBigNat#
    , wordToBigNat
    , wordToBigNat2
    , bigNatToInt
    , bigNatToWord
    , indexBigNat#

      -- ** 'BigNat' arithmetic operations
    , plusBigNat
    , plusBigNatWord
    , minusBigNat
    , minusBigNatWord
    , timesBigNat
    , timesBigNatWord
    , sqrBigNat

    , quotRemBigNat
    , quotRemBigNatWord
    , quotBigNatWord
    , quotBigNat
    , remBigNat
    , remBigNatWord

    , gcdBigNat
    , gcdBigNatWord

    , powModBigNat
    , powModBigNatWord

    , recipModBigNat

      -- ** 'BigNat' logic operations
    , shiftRBigNat
    , shiftLBigNat
    , testBitBigNat
    , andBigNat
    , xorBigNat
    , popCountBigNat
    , orBigNat
    , bitBigNat

      -- ** 'BigNat' comparison predicates
    , isZeroBigNat
    , isNullBigNat#

    , compareBigNatWord
    , compareBigNat
    , eqBigNatWord
    , eqBigNatWord#
    , eqBigNat
    , eqBigNat#
    , gtBigNatWord#

      -- * Miscellaneous GMP-provided operations
    , gcdInt
    , gcdWord
    , powModWord
    , recipModWord

      -- * Primality tests
    , testPrimeInteger
    , testPrimeBigNat
    , testPrimeWord#

    , nextPrimeInteger
    , nextPrimeBigNat
    , nextPrimeWord#

      -- * Import/export functions
      -- ** Compute size of serialisation
    , sizeInBaseBigNat
    , sizeInBaseInteger
    , sizeInBaseWord#

      -- ** Export
    , exportBigNatToAddr
    , exportIntegerToAddr
    , exportWordToAddr

    , exportBigNatToMutableByteArray
    , exportIntegerToMutableByteArray
    , exportWordToMutableByteArray

      -- ** Import

    , importBigNatFromAddr
    , importIntegerFromAddr

    , importBigNatFromByteArray
    , importIntegerFromByteArray
    ) where

import GHC.Integer.Type
import GHC.Integer
import GHC.Prim
import GHC.Types

default ()


-- | Compute number of digits (without sign) in given @/base/@.
--
-- This function wraps @mpz_sizeinbase()@ which has some
-- implementation pecularities to take into account:
--
-- * \"@'sizeInBaseInteger' 0 /base/ = 1@\"
--   (see also comment in 'exportIntegerToMutableByteArray').
--
-- * This function is only defined if @/base/ >= 2#@ and @/base/ <= 256#@
--   (Note: the documentation claims that only @/base/ <= 62#@ is
--   supported, however the actual implementation supports up to base 256).
--
-- * If @/base/@ is a power of 2, the result will be exact. In other
--   cases (e.g. for @/base/ = 10#@), the result /may/ be 1 digit too large
--   sometimes.
--
-- * \"@'sizeInBaseInteger' /i/ 2#@\" can be used to determine the most
--   significant bit of @/i/@.
--
-- @since 0.5.1.0
sizeInBaseInteger :: Integer -> Int# -> Word#
sizeInBaseInteger (S# i#)  = sizeInBaseWord# (int2Word# (absI# i#))
sizeInBaseInteger (Jp# bn) = sizeInBaseBigNat bn
sizeInBaseInteger (Jn# bn) = sizeInBaseBigNat bn

-- | Version of 'sizeInBaseInteger' operating on 'BigNat'
--
-- @since 1.0.0.0
sizeInBaseBigNat :: BigNat -> Int# -> Word#
sizeInBaseBigNat bn@(BN# ba#) = c_mpn_sizeinbase# ba# (sizeofBigNat# bn)

c_mpn_sizeinbase# :: ByteArray# -> GmpSize# -> Int# -> Word#
c_mpn_sizeinbase# = c_mpn_sizeinbase#

-- | Version of 'sizeInBaseInteger' operating on 'Word#'
--
-- @since 1.0.0.0
sizeInBaseWord# :: Word# -> Int# -> Word#
sizeInBaseWord# = sizeInBaseWord#

-- | Dump 'Integer' (without sign) to @/addr/@ in base-256 representation.
--
-- @'exportIntegerToAddr' /i/ /addr/ /e/@
--
-- See description of 'exportIntegerToMutableByteArray' for more details.
--
-- @since 1.0.0.0
exportIntegerToAddr :: Integer -> Addr# -> Int# -> IO Word
exportIntegerToAddr (S# i#)  = exportWordToAddr (W# (int2Word# (absI# i#)))
exportIntegerToAddr (Jp# bn) = exportBigNatToAddr bn
exportIntegerToAddr (Jn# bn) = exportBigNatToAddr bn

-- | Version of 'exportIntegerToAddr' operating on 'BigNat's.
exportBigNatToAddr :: BigNat -> Addr# -> Int# -> IO Word
exportBigNatToAddr bn@(BN# ba#) addr e
  = c_mpn_exportToAddr# ba# (sizeofBigNat# bn) addr 0# e

c_mpn_exportToAddr# :: ByteArray# -> GmpSize# -> Addr# -> Int# -> Int#
                         -> IO Word
c_mpn_exportToAddr# = c_mpn_exportToAddr#

-- | Version of 'exportIntegerToAddr' operating on 'Word's.
exportWordToAddr :: Word -> Addr# -> Int# -> IO Word
exportWordToAddr (W# w#) addr
  = c_mpn_export1ToAddr# w# addr 0# -- TODO: we don't calling GMP for that

c_mpn_export1ToAddr# :: GmpLimb# -> Addr# -> Int# -> Int#
                          -> IO Word
c_mpn_export1ToAddr# = c_mpn_export1ToAddr#

-- | Dump 'Integer' (without sign) to mutable byte-array in base-256
-- representation.
--
-- The call
--
-- @'exportIntegerToMutableByteArray' /i/ /mba/ /offset/ /msbf/@
--
-- writes
--
-- * the 'Integer' @/i/@
--
-- * into the 'MutableByteArray#' @/mba/@ starting at @/offset/@
--
-- * with most significant byte first if @msbf@ is @1#@ or least
--   significant byte first if @msbf@ is @0#@, and
--
-- * returns number of bytes written.
--
-- Use \"@'sizeInBaseInteger' /i/ 256#@\" to compute the exact number of
-- bytes written in advance for @/i/ /= 0@. In case of @/i/ == 0@,
-- 'exportIntegerToMutableByteArray' will write and report zero bytes
-- written, whereas 'sizeInBaseInteger' report one byte.
--
-- It's recommended to avoid calling 'exportIntegerToMutableByteArray' for small
-- integers as this function would currently convert those to big
-- integers in msbf to call @mpz_export()@.
--
-- @since 1.0.0.0
exportIntegerToMutableByteArray :: Integer -> MutableByteArray# RealWorld
                                -> Word# -> Int# -> IO Word
exportIntegerToMutableByteArray (S# i#)
    = exportWordToMutableByteArray (W# (int2Word# (absI# i#)))
exportIntegerToMutableByteArray (Jp# bn) = exportBigNatToMutableByteArray bn
exportIntegerToMutableByteArray (Jn# bn) = exportBigNatToMutableByteArray bn

-- | Version of 'exportIntegerToMutableByteArray' operating on 'BigNat's.
--
-- @since 1.0.0.0
exportBigNatToMutableByteArray :: BigNat -> MutableByteArray# RealWorld -> Word#
                               -> Int# -> IO Word
exportBigNatToMutableByteArray bn@(BN# ba#)
  = c_mpn_exportToMutableByteArray# ba# (sizeofBigNat# bn)

c_mpn_exportToMutableByteArray# :: ByteArray# -> GmpSize#
                                  -> MutableByteArray# RealWorld -> Word#
                                  -> Int# -> IO Word
c_mpn_exportToMutableByteArray# = c_mpn_exportToMutableByteArray#

-- | Version of 'exportIntegerToMutableByteArray' operating on 'Word's.
--
-- @since 1.0.0.0
exportWordToMutableByteArray :: Word -> MutableByteArray# RealWorld -> Word#
                             -> Int# -> IO Word
exportWordToMutableByteArray (W# w#) = c_mpn_export1ToMutableByteArray# w#

c_mpn_export1ToMutableByteArray# :: GmpLimb# -> MutableByteArray# RealWorld
                                   -> Word# -> Int# -> IO Word
c_mpn_export1ToMutableByteArray# = c_mpn_export1ToMutableByteArray#


-- | Probalistic Miller-Rabin primality test.
--
-- \"@'testPrimeInteger' /n/ /k/@\" determines whether @/n/@ is prime
-- and returns one of the following results:
--
-- * @2#@ is returned if @/n/@ is definitely prime,
--
-- * @1#@ if @/n/@ is a /probable prime/, or
--
-- * @0#@ if @/n/@ is definitely not a prime.
--
-- The @/k/@ argument controls how many test rounds are performed for
-- determining a /probable prime/. For more details, see
-- <http://gmplib.org/manual/Number-Theoretic-Functions.html#index-mpz_005fprobab_005fprime_005fp-360 GMP documentation for `mpz_probab_prime_p()`>.
--
-- @since 0.5.1.0
{-# NOINLINE testPrimeInteger #-}
testPrimeInteger :: Integer -> Int# -> Int#
testPrimeInteger (S# i#) = testPrimeWord# (int2Word# (absI# i#))
testPrimeInteger (Jp# n) = testPrimeBigNat n
testPrimeInteger (Jn# n) = testPrimeBigNat n

-- | Version of 'testPrimeInteger' operating on 'BigNat's
--
-- @since 1.0.0.0
testPrimeBigNat :: BigNat -> Int# -> Int#
testPrimeBigNat bn@(BN# ba#) = c_integer_gmp_test_prime# ba# (sizeofBigNat# bn)

c_integer_gmp_test_prime# :: ByteArray# -> GmpSize# -> Int# -> Int#
c_integer_gmp_test_prime# = c_integer_gmp_test_prime#

-- | Version of 'testPrimeInteger' operating on 'Word#'s
--
-- @since 1.0.0.0
testPrimeWord# :: GmpLimb# -> Int# -> Int#
testPrimeWord# = testPrimeWord#


-- | Compute next prime greater than @/n/@ probalistically.
--
-- According to the GMP documentation, the underlying function
-- @mpz_nextprime()@ \"uses a probabilistic algorithm to identify
-- primes. For practical purposes it's adequate, the chance of a
-- composite passing will be extremely small.\"
--
-- @since 0.5.1.0
{-# NOINLINE nextPrimeInteger #-}
nextPrimeInteger :: Integer -> Integer
nextPrimeInteger (S# i#)
  | isTrue# (i# ># 1#)    = wordToInteger (nextPrimeWord# (int2Word# i#))
  | True                  = S# 2#
nextPrimeInteger (Jp# bn) = Jp# (nextPrimeBigNat bn)
nextPrimeInteger (Jn# _)  = S# 2#

-- | Version of 'nextPrimeInteger' operating on 'Word#'s
--
-- @since 1.0.0.0
nextPrimeWord# :: GmpLimb# -> GmpLimb#
nextPrimeWord# = nextPrimeWord#
