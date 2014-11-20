{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE RoleAnnotations            #-}
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
-- There is no way to store the state in a seed because then it wouldn't
-- be unique anymore. Also, generators can't be initialized in ST because
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
-- | Guarantees the stream to be unique by using the pointer
--   reference as the stream. There is no way to store the state
--   in a seed because then it wouldn't be unique anymore.

module System.Random.PCG.Unique
  ( -- * Gen
    UGen, create, initialize

    -- * Getting random numbers
  , uniform, uniformB, advance
  ) where

import Control.Monad.Primitive
import Foreign

-- | Standard initial seed.
uSeed :: Word64
uSeed = 0x4d595df4d0f33173

-- | Create a 'Gen' from a fixed initial seed.
create :: IO UGen
create =  unsafePrimToPrim $ do
  p <- malloc
  poke p uSeed
  return (UGen p)

------------------------------------------------------------------------
-- UGen
------------------------------------------------------------------------

-- | State of the random number generator
newtype UGen = UGen (Ptr Word64)

-- | Create a generator from two words. Note: this is not the same as the
--   two words in a 'Seed'.
initialize :: Word64 -> IO UGen
initialize a = do
  p <- malloc
  pcg32u_srandom_r p a
  return (UGen p)

-- | Generate a uniform 'Word32' from a 'UGen'.
uniform :: UGen -> IO Word32
uniform (UGen p) = pcg32u_random_r p
{-# INLINE uniform #-}

-- | Generate a uniform 'Word32' bounded above by the given bound.
uniformB :: Word32 -> UGen -> IO Word32
uniformB u (UGen p) = unsafePrimToPrim $ pcg32u_boundedrand_r p u
{-# INLINE uniformB #-}

advance :: Word64 -> UGen -> IO ()
advance u (UGen p) = unsafePrimToPrim $ pcg32u_advance_r p u
{-# INLINE advance #-}

------------------------------------------------------------------------
-- Foreign calls
------------------------------------------------------------------------

foreign import ccall unsafe "pcg_unique_64_srandom_r"
  pcg32u_srandom_r :: Ptr Word64 -> Word64 -> IO ()

foreign import ccall unsafe "pcg_unique_64_xsh_rs_32_random_r"
  pcg32u_random_r :: Ptr Word64 -> IO Word32

foreign import ccall unsafe "pcg_unique_64_xsh_rs_32_boundedrand_r"
  pcg32u_boundedrand_r :: Ptr Word64 -> Word32 -> IO Word32

foreign import ccall unsafe "pcg_unique_64_advance_r"
  pcg32u_advance_r :: Ptr Word64 -> Word64 -> IO ()
