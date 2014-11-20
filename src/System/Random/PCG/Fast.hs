{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations            #-}
-- |
-- Module     : System.Random.PCG.Fast
-- Copyright  : Copyright (c) 2014, Christopher Chalmers <c.chalmers@me.com>
-- License    : BSD3
-- Maintainer : Christopher Chalmers <c.chalmers@me.com>
-- Stability  : experimental
-- Portability: CPP, FFI
-- Tested with: GHC 7.8.3
--
-- Fast variant of the PCG random number generator. This module performs
-- around 20% faster the multiple streams version but produces lower
-- quality (still good) random numbers.
--
-- See http://www.pcg-random.org for details.
--
-- @
-- import Control.Monad.ST
-- import System.Random.PCG.Fast
--
-- three :: [Word32]
-- three = runST $ do
--   g <- create
--   a <- uniformB 10 g
--   b <- uniformB 20 g
--   c <- uniformB 30 g
--   return [a,b,c]
-- @
module System.Random.PCG.Fast
  ( -- * Gen
    FGen
  , create, initialize

    -- * Getting random numbers
  , uniform, uniformB, advance

    -- * Seeds
  , FSeed, save, restore, fSeed, mkFSeed
  ) where

import Control.Applicative
import Control.Monad.Primitive
import Foreign
import System.IO.Unsafe

------------------------------------------------------------------------
-- Seed
------------------------------------------------------------------------

newtype FSeed = FSeed Word64
  deriving (Show, Eq, Ord, Storable)

-- | Save the state of a 'FGen' in a 'Seed'.
save :: PrimMonad m => FGen (PrimState m) -> m FSeed
save (FGen p) = unsafePrimToPrim (peek p)
{-# INLINE save #-}

-- | Restore a 'FGen' from a 'Seed'.
restore :: PrimMonad m => FSeed -> m (FGen (PrimState m))
restore s = unsafePrimToPrim $ do
  p <- malloc
  poke p s
  return (FGen p)
{-# INLINE restore #-}

mkFSeed :: Word64 -> FSeed
mkFSeed w = unsafePerformIO $ do
  p <- malloc
  pcg32f_srandom_r p w
  peek p <* free p
{-# INLINE mkFSeed #-}

-- | Standard initial seed.
fSeed :: FSeed
fSeed = FSeed 0xcafef00dd15ea5e5

-- | Create a 'Gen' from a fixed initial seed.
create :: PrimMonad m => m (FGen (PrimState m))
create = restore fSeed

------------------------------------------------------------------------
-- FGen
------------------------------------------------------------------------

-- | State of the random number generator
newtype FGen s = FGen (Ptr FSeed)
type role FGen representational

-- | Create a generator from two words. Note: this is not the same as the
--   two words in a 'Seed'.
initialize :: PrimMonad m => Word64 -> m (FGen (PrimState m))
initialize a = unsafePrimToPrim $ do
  p <- malloc
  pcg32f_srandom_r p a
  return (FGen p)

-- | Generate a uniform 'Word32' from a 'FGen'.
uniform :: PrimMonad m => FGen (PrimState m) -> m Word32
uniform (FGen p) = unsafePrimToPrim $ pcg32f_random_r p
{-# INLINE uniform #-}

-- | Generate a uniform 'Word32' bounded by the given bound.
uniformB :: PrimMonad m => Word32 -> FGen (PrimState m) -> m Word32
uniformB u (FGen p) = unsafePrimToPrim $ pcg32f_boundedrand_r p u
{-# INLINE uniformB #-}

advance :: PrimMonad m => Word64 -> FGen (PrimState m) -> m ()
advance u (FGen p) = unsafePrimToPrim $ pcg32f_advance_r p u
{-# INLINE advance #-}

------------------------------------------------------------------------
-- Foreign calls
------------------------------------------------------------------------

foreign import ccall unsafe "pcg_mcg_64_srandom_r"
  pcg32f_srandom_r :: Ptr FSeed -> Word64 -> IO ()

foreign import ccall unsafe "pcg_mcg_64_xsh_rs_32_random_r"
  pcg32f_random_r :: Ptr FSeed -> IO Word32

foreign import ccall unsafe "pcg_mcg_64_xsh_rs_32_boundedrand_r"
  pcg32f_boundedrand_r :: Ptr FSeed -> Word32 -> IO Word32

foreign import ccall unsafe "pcg_mcg_64_advance_r"
  pcg32f_advance_r :: Ptr FSeed -> Word64 -> IO ()
