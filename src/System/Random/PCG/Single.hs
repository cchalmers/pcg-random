{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations            #-}
--------------------------------------------------------------------
-- |
-- Module     : System.Random.PCG.Single
-- Copyright  : Copyright (c) 2014, Christopher Chalmers <c.chalmers@me.com>
-- License    : BSD3
-- Maintainer : Christopher Chalmers <c.chalmers@me.com>
-- Stability  : experimental
-- Portability: CPP, FFI
-- Tested with: GHC 7.8.3
--
-- Minimal iterface to the PCG Random Number Generator. See
-- http://www.pcg-random.org
--
-- @
-- import Control.Monad.ST
-- import System.Random.PCG.Single
--
-- three :: [Word32]
-- three = runST $ do
--   g <- create
--   a <- uniformB 10 g
--   b <- uniformB 20 g
--   c <- uniformB 30 g
--   return [a,b,c]
-- @
module System.Random.PCG.Single
  ( -- * Gen
    SGen
  , create, initialize

    -- * Getting random numbers
  , uniform, uniformB, advance

    -- * Seeds
  , SSeed, save, restore, sSeed, mkSSeed
  ) where

import Control.Applicative
import Control.Monad.Primitive
import Foreign
import System.IO.Unsafe

------------------------------------------------------------------------
-- Seed
------------------------------------------------------------------------

newtype SSeed = SSeed Word64
  deriving (Show, Eq, Ord, Storable)

-- | Save the state of a 'SGen' in a 'Seed'.
save :: PrimMonad m => SGen (PrimState m) -> m SSeed
save (SGen p) = unsafePrimToPrim (peek p)
{-# INLINE save #-}

-- | Restore a 'SGen' from a 'Seed'.
restore :: PrimMonad m => SSeed -> m (SGen (PrimState m))
restore s = unsafePrimToPrim $ do
  p <- malloc
  poke p s
  return (SGen p)
{-# INLINE restore #-}

mkSSeed :: Word64 -> SSeed
mkSSeed w = unsafePerformIO $ do
  p <- malloc
  pcg32s_srandom_r p w
  peek p <* free p
{-# INLINE mkSSeed #-}

-- | Standard initial seed.
sSeed :: SSeed
sSeed = SSeed 0x4d595df4d0f33173

-- | Create a 'Gen' from a fixed initial seed.
create :: PrimMonad m => m (SGen (PrimState m))
create = restore sSeed

------------------------------------------------------------------------
-- SGen
------------------------------------------------------------------------

-- | State of the random number generator
newtype SGen s = SGen (Ptr SSeed)
type role SGen representational

-- | Create a generator from two words. Note: this is not the same as the
--   two words in a 'Seed'.
initialize :: PrimMonad m => Word64 -> m (SGen (PrimState m))
initialize a = unsafePrimToPrim $ do
  p <- malloc
  pcg32s_srandom_r p a
  return (SGen p)

-- | SGenerate a uniform 'Word32' from a 'SGen'.
uniform :: PrimMonad m => SGen (PrimState m) -> m Word32
uniform (SGen p) = unsafePrimToPrim $ pcg32s_random_r p
{-# INLINE uniform #-}

-- | SGenerate a uniform 'Word32' bounded by the given bound.
uniformB :: PrimMonad m => Word32 -> SGen (PrimState m) -> m Word32
uniformB u (SGen p) = unsafePrimToPrim $ pcg32s_boundedrand_r p u
{-# INLINE uniformB #-}

advance :: PrimMonad m => Word64 -> SGen (PrimState m) -> m ()
advance u (SGen p) = unsafePrimToPrim $ pcg32s_advance_r p u
{-# INLINE advance #-}

------------------------------------------------------------------------
-- Foreign calls
------------------------------------------------------------------------

foreign import ccall unsafe "pcg_oneseq_64_srandom_r"
  pcg32s_srandom_r :: Ptr SSeed -> Word64 -> IO ()

foreign import ccall unsafe "pcg_oneseq_64_xsh_rs_32_random_r"
  pcg32s_random_r :: Ptr SSeed -> IO Word32

foreign import ccall unsafe "pcg_oneseq_64_xsh_rs_32_boundedrand_r"
  pcg32s_boundedrand_r :: Ptr SSeed -> Word32 -> IO Word32

foreign import ccall unsafe "pcg_oneseq_64_advance_r"
  pcg32s_advance_r :: Ptr SSeed -> Word64 -> IO ()
