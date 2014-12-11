{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
--------------------------------------------------------------------
-- |
-- Module     : System.Random.PCG.Unique
-- Copyright  : Copyright (c) 2014, Christopher Chalmers <c.chalmers@me.com>
-- License    : BSD3
-- Maintainer : Christopher Chalmers <c.chalmers@me.com>
-- Stability  : experimental
-- Portability: CPP, FFI
-- Tested with: GHC 7.8.3
--
-- Unique variant of the PCG random number generator. Guarantees the
-- sequence to be unique by using the pointer address to select the
-- output sequence.
--
-- There is no way to freeze the state  because then it wouldn't be
-- unique anymore. Also, generators can't be initialized in ST because
-- we don't know what pointer reference they'll get.
--
-- See http://www.pcg-random.org for details.
--
-- @
-- import System.Random.PCG.Unique
--
-- three :: IO [Word32]
-- three = do
--   g <- create
--   a <- uniformB 10 g
--   b <- uniformB 20 g
--   c <- uniformB 30 g
--   return [a,b,c]
-- @

module System.Random.PCG.Unique
  ( -- * Gen
    Gen
  , create, createSystemRandom, initialize, withSystemRandom

    -- * Getting random numbers
  , Variate (..)
  , advance, retract
  ) where

import Data.Functor
import Foreign

import System.Random.PCG.Class

-- | Standard initial seed.
seed :: Word64
seed = 0x4d595df4d0f33173

-- | Create a 'Gen' from a fixed initial seed.
create :: IO Gen
create = initialize seed
  -- do
  -- p <- malloc
  -- poke p seed
  -- return (Gen p)

  -- Note that this does produce a unique sequence but if two generators
  -- are created in sequence they'll have the similar pointer references
  -- and the first couple of numbers are likely to be the same. Since
  -- this is undesirable we run initialise to randomise it.

------------------------------------------------------------------------
-- Generator
------------------------------------------------------------------------

-- | State of the random number generator
newtype Gen = Gen (Ptr Word64)
  deriving (Eq, Ord)

-- | Create a generator from two words. Note: this is not the same as the
--   two words in a 'Seed'.
initialize :: Word64 -> IO Gen
initialize a = do
  p <- malloc
  pcg32u_srandom_r p a
  return (Gen p)

-- | Seed with system random number. (\"@\/dev\/urandom@\" on Unix-like
--   systems, time otherwise).
withSystemRandom :: (Gen -> IO a) -> IO a
withSystemRandom f = sysRandom >>= initialize >>= f

-- | Seed a PRNG with data from the system's fast source of pseudo-random
--   numbers. All the caveats of 'withSystemRandom' apply here as well.
createSystemRandom :: IO Gen
createSystemRandom = withSystemRandom return

-- -- | Generate a uniform 'Word32' bounded above by the given bound.
-- uniformB :: Word32 -> UGen -> IO Word32
-- uniformB u (UGen p) = unsafePrimToPrim $ pcg32u_boundedrand_r p u
-- {-# INLINE uniformB #-}

-- | Advance the given generator n steps in log(n) time.
advance :: Word64 -> Gen -> IO ()
advance u (Gen p) = pcg32u_advance_r p u
{-# INLINE advance #-}

-- | Retract the given generator n steps in log(2^64-n) time. This
--   is just @advance (-n)@.
retract :: Word64 -> Gen -> IO ()
retract u g = advance (-u) g
{-# INLINE retract #-}

------------------------------------------------------------------------
-- Foreign calls
------------------------------------------------------------------------

foreign import ccall unsafe "pcg_unique_64_srandom_r"
  pcg32u_srandom_r :: Ptr Word64 -> Word64 -> IO ()

foreign import ccall unsafe "pcg_unique_64_xsh_rs_32_random_r"
  pcg32u_random_r :: Ptr Word64 -> IO Word32

-- foreign import ccall unsafe "pcg_unique_64_xsh_rs_32_boundedrand_r"
--   pcg32u_boundedrand_r :: Ptr Word64 -> Word32 -> IO Word32

foreign import ccall unsafe "pcg_unique_64_advance_r"
  pcg32u_advance_r :: Ptr Word64 -> Word64 -> IO ()

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Generator Gen IO where
  uniform1 f (Gen p) = f <$> pcg32u_random_r p
  {-# INLINE uniform1 #-}

  uniform2 f (Gen p) = do
    w1 <- pcg32u_random_r p
    w2 <- pcg32u_random_r p
    return $ f w1 w2
  {-# INLINE uniform2 #-}
