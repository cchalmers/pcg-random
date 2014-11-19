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
  , Seed, save, restore, seed, uniforms
  , next1, next2, split, mkSeed
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Foreign
import System.IO.Unsafe

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

-- | Given a 'Seed', return a uniform 'Word32' with a new 'Seed'.
next1 :: Seed -> (Word32, Seed)
next1 s = unsafePerformIO $ do
  p  <- malloc
  poke p s
  w  <- pcg32_random_r p
  s' <- peek p
  free p
  return (w,s')
{-# INLINE next1 #-}

-- | Given a 'Seed', return two uniform 'Word32's with a new 'Seed'.
next2 :: Seed -> (Word32, Word32, Seed)
next2 s = unsafePerformIO $ do
  p  <- malloc
  poke p s
  w1 <- pcg32_random_r p
  w2 <- pcg32_random_r p
  s' <- peek p
  free p
  return (w1,w2,s')
{-# INLINE next2 #-}

-- | Produce an infinite stream of random words using the given seed.
uniforms :: Seed -> [Word32]
uniforms s = map unsafePerformIO $ repeat (pcg32_random_r pointer)
  where
    pointer = unsafePerformIO $ do
      p <- malloc
      poke p s
      return p

-- | Split a 'Seed' into two different seeds. The robustness of this
--   split is untested.
split :: Seed -> (Seed, Seed)
split s = unsafePerformIO $ do
  p  <- malloc
  poke p s
  w1 <- pcg32_random_r p
  w2 <- pcg32_random_r p
  w3 <- pcg32_random_r p
  w4 <- pcg32_random_r p
  w5 <- pcg32_random_r p
  w6 <- pcg32_random_r p
  w7 <- pcg32_random_r p
  w8 <- pcg32_random_r p
  pcg32_srandom_r p (com w1 w2) (com w3 w4)
  s1 <- peek p
  pcg32_srandom_r p (com w5 w6) (com w7 w8)
  s2 <- peek p
  free p
  return (s1,s2)

com :: Word32 -> Word32 -> Word64
com w1 w2 = fromIntegral w1 + fromIntegral w2
{-# INLINE com #-}

-- | Generate a new seed using two 'Word64's. Note: the words in the show
--   instance of the Seed will not be the same as the words given.
mkSeed :: Word64 -> Word64 -> Seed
mkSeed w1 w2 = unsafePerformIO $ do
  p <- malloc
  pcg32_srandom_r p w1 w2
  peek p <* free p
{-# INLINE mkSeed #-}

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
