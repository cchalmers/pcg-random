{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RoleAnnotations          #-}
--------------------------------------------------------------------
-- |
-- Module     : System.Random.PCG
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
-- Interface based on mwc-random.
--
-- @
-- import Control.Monad.ST
-- import System.Random.PCG
--
-- three :: [Word32]
-- three = runST $ do
--   g <- create
--   a <- uniformB 10 g
--   b <- uniformB 20 g
--   c <- uniformB 30 g
--   return [a,b,c]
-- @

module System.Random.PCG
  ( -- * Gen
    Gen, IOGen, STGen
  , create, initialize

    -- * Getting random numbers
  , uniform, uniformB, uniformR

    -- * Seeds
  , Seed, save, restore
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Foreign

------------------------------------------------------------------------
-- State
------------------------------------------------------------------------

-- | Immutable snapshot of the state of a 'Gen'.
data Seed = Seed {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Show, Eq, Ord)

-- | Save the state of a 'Gen' in a 'Seed'.
save :: PrimMonad m => Gen (PrimState m) -> m Seed
save (Gen p) = unsafePrimToPrim (peek p)
{-# INLINE save #-}

-- | Restore a 'Gen' from a 'Seed'.
restore :: PrimMonad m => Seed -> m (Gen (PrimState m))
restore s = unsafePrimToPrim $ do
  p <- malloc
  poke p s
  return (Gen p)
{-# INLINE restore #-}

-- pcg internals are ment to be private but it looks like this works
instance Storable Seed where
  sizeOf _ = 16
  {-# INLINE sizeOf #-}
  alignment _ = 8
  {-# INLINE alignment #-}
  poke ptr (Seed x y) = poke ptr' x >> pokeElemOff ptr' 1 y
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = Seed <$> peek ptr' <*> peekElemOff ptr' 1
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

------------------------------------------------------------------------
-- PrimMonad interface
------------------------------------------------------------------------

-- | State of the random number generator
newtype Gen s = Gen (Ptr Seed)
type role Gen representational

-- this should be type safe because the Gen cannot escape its PrimMonad

-- | Type alias of 'Gen' specialized to 'IO'.
type IOGen   = Gen (PrimState IO)

-- | Type alias of 'Gen' specialized to 'ST'.
type STGen s = Gen (PrimState (ST s))

-- | Create a generator from two words. Note: this is not the same as the
--   two words in a 'Seed'.
initialize :: PrimMonad m => Word64 -> Word64 -> m (Gen (PrimState m))
initialize a b = unsafePrimToPrim $ do
  p <- malloc
  pcg32_srandom_r p a b
  return (Gen p)

-- | Create a 'Gen' from a fixed initial seed.
create :: PrimMonad m => m (Gen (PrimState m))
create = restore seed

seed :: Seed
seed = Seed 0x853c49e6748fea9b 0xda3e39cb94b95bdb

-- | Generate a uniform 'Word32' from a 'Gen'.
uniform :: PrimMonad m => Gen (PrimState m) -> m Word32
uniform (Gen p) = unsafePrimToPrim $ pcg32_random_r p
{-# INLINE uniform #-}

-- | Generate a uniform 'Word32' bounded by the given bound.
uniformB :: PrimMonad m => Word32 -> Gen (PrimState m) -> m Word32
uniformB u (Gen p) = unsafePrimToPrim $ pcg32_boundedrand_r p u
{-# INLINE uniformB #-}

-- | Generate a uniform 'Word32' within the given bounds. Should be in the
--   form (lower, upper).
uniformR :: PrimMonad m => (Word32, Word32) -> Gen (PrimState m) -> m Word32
uniformR (l,u) g = (+l) `liftM` uniformB (u - l) g
{-# INLINE uniformR #-}

------------------------------------------------------------------------
-- Foreign calls
------------------------------------------------------------------------

-- It shouldn't be too hard to impliment the algorithm in pure haskell.
-- For now just use the pcg-c-basic interface.

foreign import ccall unsafe "pcg32_srandom_r"
  pcg32_srandom_r :: Ptr Seed -> Word64 -> Word64 -> IO ()

foreign import ccall unsafe "pcg32_random_r"
  pcg32_random_r :: Ptr Seed -> IO Word32

foreign import ccall unsafe "pcg32_boundedrand_r"
  pcg32_boundedrand_r :: Ptr Seed -> Word32 -> IO Word32
