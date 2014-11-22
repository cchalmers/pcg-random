{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RoleAnnotations            #-}
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
-- Standard PCG Random Number Generator with chosen streams. See
-- http://www.pcg-random.org for details.
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
  ( -- * Generator
    Gen, IOGen, STGen
  , create, initialize

    -- * Getting random numbers
  , Variate (..)
  , advance, retract

    -- * Frozen generator
  , FrozenGen, save, restore, seed, initFrozen
  ) where

import Control.Applicative
import Control.Monad.Primitive
import Foreign
import System.IO.Unsafe
import System.Random

import System.Random.PCG.Class

------------------------------------------------------------------------
-- State
------------------------------------------------------------------------

-- | Immutable snapshot of the state of a 'Gen'.
data FrozenGen = FrozenGen {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Show, Eq, Ord)

-- | Save the state of a 'Gen' in a 'FrozenGen'.
save :: PrimMonad m => Gen (PrimState m) -> m FrozenGen
save (Gen p) = unsafePrimToPrim (peek p)
{-# INLINE save #-}

-- | Restore a 'Gen' from a 'FrozenGen'.
restore :: PrimMonad m => FrozenGen -> m (Gen (PrimState m))
restore s = unsafePrimToPrim $ do
  p <- malloc
  poke p s
  return (Gen p)
{-# INLINE restore #-}

-- -- | Produce an infinite stream of random words using the given seed.
-- uniforms :: FrozenGen -> [Word32]
-- uniforms s = map unsafePerformIO $ repeat (pcg32_random_r pointer)
--   where
--     pointer = unsafePerformIO $ do
--       p <- malloc
--       poke p s
--       return p

-- | Generate a new seed using two 'Word64's. Note: the words in the show
--   instance of the FrozenGen will not be the same as the words given.
initFrozen :: Word64 -> Word64 -> FrozenGen
initFrozen w1 w2 = unsafePerformIO $ do
  p <- malloc
  pcg32_srandom_r p w1 w2
  peek p <* free p
{-# INLINE initFrozen #-}

instance Storable FrozenGen where
  sizeOf _ = 16
  {-# INLINE sizeOf #-}
  alignment _ = 8
  {-# INLINE alignment #-}
  poke ptr (FrozenGen x y) = poke ptr' x >> pokeElemOff ptr' 1 y
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = FrozenGen <$> peek ptr' <*> peekElemOff ptr' 1
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

------------------------------------------------------------------------
-- PrimMonad interface
------------------------------------------------------------------------

-- | State of the random number generator
newtype Gen s = Gen (Ptr FrozenGen)
  deriving (Eq, Ord)
type role Gen representational

-- this should be type safe because the Gen cannot escape its PrimMonad

-- | Type alias of 'Gen' specialized to 'IO'.
type IOGen = Gen RealWorld

-- | Type alias of 'Gen' specialized to 'ST'. (
type STGen s = Gen s
-- Note this doesn't force it to be in ST. You can write (STGen Realworld)
-- and it'll work in IO. Writing STGen s = Gen (PrimState (ST s)) doesn't
-- solve this.

-- | Create a 'Gen' from a fixed initial seed.
create :: PrimMonad m => m (Gen (PrimState m))
create = restore seed

seed :: FrozenGen
seed = FrozenGen 0x853c49e6748fea9b 0xda3e39cb94b95bdb

-- | FrozenGen a generator with two words. The first is the position in the
--   stream and the second which stream to use.
initialize :: PrimMonad m => Word64 -> Word64 -> m (Gen (PrimState m))
initialize a b = unsafePrimToPrim $ do
  p <- malloc
  pcg32_srandom_r p a b
  return (Gen p)

-- -- | Generate a uniform 'Word32' bounded by the given bound.
-- uniformB :: PrimMonad m => Word32 -> Gen (PrimState m) -> m Word32
-- uniformB u (Gen p) = unsafePrimToPrim $ pcg32_boundedrand_r p u
-- {-# INLINE uniformB #-}

-- | Advance the given generator n steps in log(n) time.
advance :: PrimMonad m => Word64 -> Gen (PrimState m) -> m ()
advance u (Gen p) = unsafePrimToPrim $ pcg32_advance_r p u
{-# INLINE advance #-}

-- | Retract the given generator n steps in log(2^64-n) time. This
--   is just @advance (-n)@.
retract :: PrimMonad m => Word64 -> Gen (PrimState m) -> m ()
retract u g = advance (-u) g
{-# INLINE retract #-}

------------------------------------------------------------------------
-- Foreign calls
------------------------------------------------------------------------

-- It shouldn't be too hard to impliment the algorithm in pure haskell.
-- For now just use the c interface.

-- For whatever reason, calling the #defined versions doesn't seem to work
-- so we need to call the low-level api directly

foreign import ccall unsafe "pcg_setseq_64_srandom_r"
  pcg32_srandom_r :: Ptr FrozenGen -> Word64 -> Word64 -> IO ()

foreign import ccall unsafe "pcg_setseq_64_xsh_rr_32_random_r"
  pcg32_random_r :: Ptr FrozenGen -> IO Word32

-- foreign import ccall unsafe "pcg_setseq_64_xsh_rr_32_boundedrand_r"
--   pcg32_boundedrand_r :: Ptr FrozenGen -> Word32 -> IO Word32

foreign import ccall unsafe "pcg_setseq_64_advance_r"
  pcg32_advance_r :: Ptr FrozenGen -> Word64 -> IO ()

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance (PrimMonad m, s ~ PrimState m) => Generator (Gen s) m where
  uniform1 f (Gen p) = unsafePrimToPrim $ f <$> pcg32_random_r p
  {-# INLINE uniform1 #-}

  uniform2 f (Gen p) = unsafePrimToPrim $ do
    w1 <- pcg32_random_r p
    w2 <- pcg32_random_r p
    return $ f w1 w2
  {-# INLINE uniform2 #-}

instance RandomGen FrozenGen where
  next s = unsafeDupablePerformIO $ do
    p <- malloc
    poke p s
    w1 <- pcg32_random_r p
    w2 <- pcg32_random_r p
    s' <- peek p
    free p
    return (wordsTo64Bit w1 w2, s')
  {-# INLINE next #-}

  split s = unsafePerformIO $ do
    p <- malloc
    poke p s
    w1 <- pcg32_random_r p
    w2 <- pcg32_random_r p
    w3 <- pcg32_random_r p
    w4 <- pcg32_random_r p
    w5 <- pcg32_random_r p
    w6 <- pcg32_random_r p
    w7 <- pcg32_random_r p
    w8 <- pcg32_random_r p
    pcg32_srandom_r p (wordsTo64Bit w1 w2) (wordsTo64Bit w3 w4)
    s1 <- peek p
    pcg32_srandom_r p (wordsTo64Bit w5 w6) (wordsTo64Bit w7 w8)
    s2 <- peek p
    free p
    return (s1,s2)

