{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE RoleAnnotations            #-}
#endif
--------------------------------------------------------------------
-- |
-- Module     : System.Random.PCG.Single
-- Copyright  : Copyright (c) 2014, Christopher Chalmers <c.chalmers@me.com>
-- License    : BSD3
-- Maintainer : Christopher Chalmers <c.chalmers@me.com>
-- Stability  : experimental
-- Portability: CPP, FFI
--
-- Single variant of the PCG random number generator. This module only
-- uses a single stream. See <http://www.pcg-random.org> for details.
--
-- @
-- import Control.Monad.ST
-- import System.Random.PCG.Single
--
-- three :: [Double]
-- three = runST $ do
--   g <- create
--   a <- uniform g
--   b <- uniform g
--   c <- uniform g
--   return [a,b,c]
-- @
module System.Random.PCG.Single
  ( -- * Gen
    Gen, GenIO, GenST
  , create, createSystemRandom, initialize, withSystemRandom

    -- * Getting random numbers
  , Variate (..)
  , advance, retract

    -- * Seeds
  , FrozenGen
  , save, restore, seed, initFrozen

    -- * Type restricted versions
  , uniformW8, uniformW16, uniformW32, uniformW64
  , uniformI8, uniformI16, uniformI32, uniformI64
  , uniformF, uniformD, uniformBool
  ) where

import Control.Applicative
import Control.Monad.Primitive
import Data.Data
import Foreign
import GHC.Generics
import System.IO.Unsafe
import System.Random

import System.Random.PCG.Class

-- $setup
-- >>> import System.Random.PCG.Single
-- >>> import System.Random.PCG.Class
-- >>> import Control.Monad

------------------------------------------------------------------------
-- Seed
------------------------------------------------------------------------

newtype FrozenGen = FrozenGen Word64
  deriving (Show, Eq, Ord, Storable, Data, Typeable, Generic)

-- | Save the state of a 'Gen' in a 'Seed'.
save :: PrimMonad m => Gen (PrimState m) -> m FrozenGen
save (Gen p) = unsafePrimToPrim (peek p)
{-# INLINE save #-}

-- | Restore a 'Gen' from a 'Seed'.
restore :: PrimMonad m => FrozenGen -> m (Gen (PrimState m))
restore s = unsafePrimToPrim $ do
  p <- malloc
  poke p s
  return (Gen p)
{-# INLINE restore #-}

-- | Generate a new seed using single 'Word64'.
--
--   >>> initFrozen 0
--   FrozenGen 1876011003808476466
initFrozen :: Word64 -> FrozenGen
initFrozen w = unsafeDupablePerformIO . alloca $ \p ->
  pcg32s_srandom_r p w >> peek p
{-# INLINE initFrozen #-}

-- | Standard initial seed.
seed :: FrozenGen
seed = FrozenGen 0x4d595df4d0f33173

-- | Create a 'Gen' from a fixed initial seed.
create :: PrimMonad m => m (Gen (PrimState m))
create = restore seed

------------------------------------------------------------------------
-- Gen
------------------------------------------------------------------------

-- | State of the random number generator
newtype Gen s = Gen (Ptr FrozenGen)
  deriving (Eq, Ord)
#if __GLASGOW_HASKELL__ >= 707
type role Gen representational
#endif

type GenIO = Gen RealWorld
type GenST = Gen

-- | Initialize a generator a single word.
--
--   >>> initialize 0 >>= save
--   FrozenGen 1876011003808476466
initialize :: PrimMonad m => Word64 -> m (Gen (PrimState m))
initialize a = unsafePrimToPrim $ do
  p <- malloc
  pcg32s_srandom_r p a
  return (Gen p)

-- | Seed with system random number. (\"@\/dev\/urandom@\" on Unix-like
--   systems, time otherwise).
withSystemRandom :: (GenIO -> IO a) -> IO a
withSystemRandom f = sysRandom >>= initialize >>= f

-- | Seed a PRNG with data from the system's fast source of pseudo-random
--   numbers. All the caveats of 'withSystemRandom' apply here as well.
createSystemRandom :: IO GenIO
createSystemRandom = withSystemRandom return

-- -- | Generate a uniform 'Word32' bounded by the given bound.
-- uniformB :: PrimMonad m => Word32 -> Gen (PrimState m) -> m Word32
-- uniformB u (Gen p) = unsafePrimToPrim $ pcg32s_boundedrand_r p u
-- {-# INLINE uniformB #-}

-- | Advance the given generator n steps in log(n) time. (Note that a
--   \"step\" is a single random 32-bit (or less) 'Variate'. Data types
--   such as 'Double' or 'Word64' require two \"steps\".)
--
--   >>> create >>= \g -> replicateM_ 1000 (uniformW32 g) >> uniformW32 g
--   787992099
--   >>> create >>= \g -> replicateM_ 500 (uniformD g) >> uniformW32 g
--   787992099
--   >>> create >>= \g -> advance 1000 g >> uniformW32 g
--   787992099
advance :: PrimMonad m => Word64 -> Gen (PrimState m) -> m ()
advance u (Gen p) = unsafePrimToPrim $ pcg32s_advance_r p u
{-# INLINE advance #-}

-- | Retract the given generator n steps in log(2^64-n) time. This
--   is just @advance (-n)@.
--
--   >>> create >>= \g -> replicateM 3 (uniformW32 g)
--   [1499251125,2252584727,2021901712]
--   >>> create >>= \g -> retract 1 g >> replicateM 3 (uniformW32 g)
--   [4031155981,1499251125,2252584727]
retract :: PrimMonad m => Word64 -> Gen (PrimState m) -> m ()
retract u g = advance (-u) g
{-# INLINE retract #-}

------------------------------------------------------------------------
-- Foreign calls
------------------------------------------------------------------------

foreign import ccall unsafe "pcg_oneseq_64_srandom_r"
  pcg32s_srandom_r :: Ptr FrozenGen -> Word64 -> IO ()

foreign import ccall unsafe "pcg_oneseq_64_xsh_rs_32_random_r"
  pcg32s_random_r :: Ptr FrozenGen -> IO Word32

-- foreign import ccall unsafe "pcg_oneseq_64_xsh_rs_32_boundedrand_r"
--   pcg32s_boundedrand_r :: Ptr FrozenGen -> Word32 -> IO Word32

foreign import ccall unsafe "pcg_oneseq_64_advance_r"
  pcg32s_advance_r :: Ptr FrozenGen -> Word64 -> IO ()

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance (PrimMonad m, s ~ PrimState m) => Generator (Gen s) m where
  uniform1 f (Gen p) = unsafePrimToPrim $
    f <$> pcg32s_random_r p
  {-# INLINE uniform1 #-}

  uniform2 f (Gen p) = unsafePrimToPrim $
    f <$> pcg32s_random_r p <*> pcg32s_random_r p
  {-# INLINE uniform2 #-}

instance RandomGen FrozenGen where
  next s = unsafeDupablePerformIO $ do
    p <- malloc
    poke p s
    w1 <- pcg32s_random_r p
    w2 <- pcg32s_random_r p
    s' <- peek p
    free p
    return (wordsTo64Bit w1 w2, s')
  {-# INLINE next #-}

  split s = unsafeDupablePerformIO $ do
    p  <- malloc
    poke p s
    w1 <- pcg32s_random_r p
    w2 <- pcg32s_random_r p
    w3 <- pcg32s_random_r p
    w4 <- pcg32s_random_r p
    pcg32s_srandom_r p (wordsTo64Bit w1 w2)
    s1 <- peek p
    pcg32s_srandom_r p (wordsTo64Bit w3 w4)
    s2 <- peek p
    free p
    return (s1,s2)

