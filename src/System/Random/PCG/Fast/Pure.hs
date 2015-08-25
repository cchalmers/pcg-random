{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnboxedTuples              #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE RoleAnnotations            #-}
#endif
-- |
-- Module     : System.Random.PCG.Fast.Pure
-- Copyright  : Copyright (c) 2015, Christopher Chalmers <c.chalmers@me.com>
-- License    : BSD3
-- Maintainer : Christopher Chalmers <c.chalmers@me.com>
-- Stability  : experimental
-- Portability: CPP, FFI
--
-- Fast variant of the PCG random number generator. This module performs
-- around 20% faster than the multiple streams version but produces slightly
-- lower quality (still good) random numbers.
--
-- See <http://www.pcg-random.org> for details.
--
-- @
-- import Control.Monad.ST
-- import System.Random.PCG.Fast
--
-- three :: [Double]
-- three = runST $ do
--   g <- create
--   a <- uniform g
--   b <- uniform g
--   c <- uniform g
--   return [a,b,c]
-- @
module System.Random.PCG.Fast.Pure
  ( -- * Gen
    Gen, GenIO, GenST
  , create, createSystemRandom, initialize, withSystemRandom

    -- * Getting random numbers
  , Variate (..)
  , advance, retract

    -- * Seeds
  , FrozenGen, save, restore, seed, initFrozen

    -- * Type restricted versions
    -- ** uniform
  , uniformW8, uniformW16, uniformW32, uniformW64
  , uniformI8, uniformI16, uniformI32, uniformI64
  , uniformF, uniformD, uniformBool

    -- ** uniformR
  , uniformRW8, uniformRW16, uniformRW32, uniformRW64
  , uniformRI8, uniformRI16, uniformRI32, uniformRI64
  , uniformRF, uniformRD, uniformRBool

    -- ** uniformB
  , uniformBW8, uniformBW16, uniformBW32, uniformBW64
  , uniformBI8, uniformBI16, uniformBI32, uniformBI64
  , uniformBF, uniformBD, uniformBBool
  ) where

import Control.Monad.Primitive
import Data.Bits
import Data.Primitive.ByteArray
import Data.Primitive.Types
import GHC.Word

import System.Random
import System.Random.PCG.Class

newtype FrozenGen = F Word64
  deriving (Show, Eq, Ord, Prim)

newtype Gen s = G (MutableByteArray s)

type GenIO = Gen RealWorld
type GenST = Gen

-- $setup
-- >>> import System.Random.PCG.Fast.Pure
-- >>> import System.Random.PCG.Class
-- >>> import Control.Monad

-- internals -----------------------------------------------------------

data Pair = P {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word32
  deriving Show

fastMultiplier :: Word64
fastMultiplier = 6364136223846793005

-- Compute the next state of the generator
state :: Word64 -> Word64
state s = s * fastMultiplier

-- Compute the output random number from the state of the generator.
output :: Word64 -> Word32
output s = fromIntegral $
  ((s `shiftR` 22) `xor` s) `unsafeShiftR` (fromIntegral (s `shiftR` 61) + 22)

-- Compute the next state and output in a strict pair.
pair :: Word64 -> Pair
pair s = P (state s) (output s)

-- Given some bound and the generator, compute the new state and bounded
-- random number.
bounded :: Word32 -> Word64 -> Pair
bounded b s0 = go s0
  where
    t = negate b `mod` b
    go !s | r >= t    = P s' (r `mod` b)
          | otherwise = go s'
      where P s' r = pair s
{- INLINE bounded #-}

advancing
  :: Word64 -- amount to advance by
  -> Word64 -- state
  -> Word64 -- multiplier
  -> Word64 -- increment
  -> Word64 -- new state
advancing d0 s m0 p0 = go d0 m0 p0 1 0
  where
    go d cm cp am ap
      | d <= 0    = am * s + ap
      | odd d     = go d' cm' cp' (am * cm) (ap * cm + cp)
      | otherwise = go d' cm' cp' am        ap
      where
        cm' = cm * cm
        cp' = (cm + 1) * cp
        d'  = d `div` 2

advanceFast :: Word64 -> FrozenGen -> FrozenGen
advanceFast d (F s) = F $ advancing d s fastMultiplier 0

------------------------------------------------------------------------
-- Seed
------------------------------------------------------------------------

-- | Immutable state of a random number generator. Suitable for storing
--   for later use.
-- newtype FrozenGen = FrozenGen Word64
--   deriving (Show, Eq, Ord, Storable, Data, Typeable, Generic)

-- | Save the state of a 'Gen' in a 'Seed'.
save :: PrimMonad m => Gen (PrimState m) -> m FrozenGen
save (G a) = readByteArray a 0
{-# INLINE save #-}

-- | Restore a 'Gen' from a 'Seed'.
restore :: PrimMonad m => FrozenGen -> m (Gen (PrimState m))
restore f = do
  a <- newByteArray 8
  writeByteArray a 0 f
  return $! G a
{-# INLINE restore #-}

-- | Generate a new seed using single 'Word64'.
--
--   >>> initFrozen 0
--   FrozenGen 1
initFrozen :: Word64 -> FrozenGen
initFrozen w = F (w .|. 1)

-- | Standard initial seed.
seed :: FrozenGen
seed = F 0xcafef00dd15ea5e5

-- | Create a 'Gen' from a fixed initial seed.
create :: PrimMonad m => m (Gen (PrimState m))
create = restore seed

------------------------------------------------------------------------
-- Gen
------------------------------------------------------------------------

-- | State of the random number generator
-- newtype Gen s = Gen (MutableByteArray s)
--   deriving (Eq, Ord)
-- #if __GLASGOW_HASKELL__ >= 707
-- type role Gen representational
-- #endif

-- type GenIO = Gen RealWorld
-- type GenST = Gen

-- | Initialize a generator a single word.
--
--   >>> initialize 0 >>= save
--   FrozenGen 1
initialize :: PrimMonad m => Word64 -> m (Gen (PrimState m))
initialize a = restore (initFrozen a)

-- | Seed with system random number. (\"@\/dev\/urandom@\" on Unix-like
--   systems, time otherwise).
withSystemRandom :: (GenIO -> IO a) -> IO a
withSystemRandom f = do
  w <- sysRandom
  initialize w >>= f

-- | Seed a PRNG with data from the system's fast source of pseudo-random
-- numbers. All the caveats of 'withSystemRandom' apply here as well.
createSystemRandom :: IO GenIO
createSystemRandom = withSystemRandom (return :: GenIO -> IO GenIO)

-- -- | Generate a uniform 'Word32' bounded by the given bound.
-- uniformB :: PrimMonad m => Word32 -> Gen (PrimState m) -> m Word32
-- uniformB u (Gen p) = unsafePrimToPrim $ pcg32f_boundedrand_r p u
-- {-# INLINE uniformB #-}

-- | Advance the given generator n steps in log(n) time. (Note that a
--   \"step\" is a single random 32-bit (or less) 'Variate'. Data types
--   such as 'Double' or 'Word64' require two \"steps\".)
--
--   >>> create >>= \g -> replicateM_ 1000 (uniformW32 g) >> uniformW32 g
--   3725702568
--   >>> create >>= \g -> replicateM_ 500 (uniformD g) >> uniformW32 g
--   3725702568
--   >>> create >>= \g -> advance 1000 g >> uniformW32 g
--   3725702568
advance :: PrimMonad m => Word64 -> Gen (PrimState m) -> m ()
advance u (G a) = do
  s <- readByteArray a 0
  let s' = advanceFast u s
  writeByteArray a 0 s'
{-# INLINE advance #-}

-- | Retract the given generator n steps in log(2^64-n) time. This
--   is just @advance (-n)@.
--
--   >>> create >>= \g -> replicateM 3 (uniformW32 g)
--   [2951688802,2698927131,361549788]
--   >>> create >>= \g -> retract 1 g >> replicateM 3 (uniformW32 g)
--   [954135925,2951688802,2698927131]
retract :: PrimMonad m => Word64 -> Gen (PrimState m) -> m ()
retract u g = advance (-u) g
{-# INLINE retract #-}

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance (PrimMonad m, s ~ PrimState m) => Generator (Gen s) m where
  uniform1 f (G a) = do
    s <- readByteArray a 0
    let P s' r = pair s
    writeByteArray a 0 s'
    return $! f r
  {-# INLINE uniform1 #-}

  uniform2 f (G a) = do
    s <- readByteArray a 0
    let s' = state s
    writeByteArray a 0 (state s')
    return $! f (output s) (output s')
  {-# INLINE uniform2 #-}

  uniform1B f b (G a) = do
    s <- readByteArray a 0
    let P s' r = bounded b s
    writeByteArray a 0 s'
    return $! f r
  {-# INLINE uniform1B #-}

instance RandomGen FrozenGen where
  next (F s) = (wordsTo64Bit w1 w2, F s'')
    where
      P s'  w1 = pair s
      P s'' w2 = pair s'

  split (F s) = (mk w1 w2, mk w3 w4)
    where
      mk a b  = initFrozen $! wordsTo64Bit a b
      P s1 w1 = pair s
      P s2 w2 = pair s1
      P s3 w3 = pair s2
      w4 = output s3 -- abandon old state

