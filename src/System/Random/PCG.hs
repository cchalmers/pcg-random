{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE RoleAnnotations            #-}
#endif
--------------------------------------------------------------------
-- |
-- Module     : System.Random.PCG
-- Copyright  : Copyright (c) 2014, Christopher Chalmers <c.chalmers@me.com>
-- License    : BSD3
-- Maintainer : Christopher Chalmers <c.chalmers@me.com>
-- Stability  : experimental
-- Portability: CPP, FFI
--
-- Standard PCG Random Number Generator with chosen streams. See
-- <http://www.pcg-random.org> for details.
--
-- @
-- import Control.Monad.ST
-- import System.Random.PCG
--
-- three :: [Double]
-- three = runST $ do
--   g <- create
--   a <- uniform g
--   b <- uniform g
--   c <- uniform g
--   return [a,b,c]
-- @

module System.Random.PCG
  ( -- * Generator
    Gen, GenIO, GenST
  , create, createSystemRandom, initialize, withSystemRandom

    -- * Getting random numbers
  , Variate (..)
  , advance, retract

    -- * Frozen generator
  , FrozenGen
  , save, restore, seed, initFrozen

    -- * Type restricted versions
  , uniformW8, uniformW16, uniformW32, uniformW64
  , uniformI8, uniformI16, uniformI32, uniformI64
  , uniformF, uniformD, uniformBool
  ) where

import Data.Data
import Control.Applicative
import Control.Monad.Primitive
import Foreign
import GHC.Generics
import System.IO.Unsafe
import System.Random

import System.Random.PCG.Class

-- $setup
-- >>> import System.Random.PCG
-- >>> import System.Random.PCG.Class
-- >>> import Control.Monad

------------------------------------------------------------------------
-- State
------------------------------------------------------------------------

-- | Immutable snapshot of the state of a 'Gen'.
data FrozenGen = FrozenGen {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

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

-- | Fixed seed.
seed :: FrozenGen
seed = FrozenGen 0x853c49e6748fea9b 0xda3e39cb94b95bdb

-- | Generate a new seed using two 'Word64's.
--
--   >>> initFrozen 0 0
--   FrozenGen 6364136223846793006 1
initFrozen :: Word64 -> Word64 -> FrozenGen
initFrozen w1 w2 = unsafeDupablePerformIO $ do
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

#if __GLASGOW_HASKELL__ >= 707
type role Gen representational
#endif

-- this should be type safe because the Gen cannot escape its PrimMonad

-- | Type alias of 'Gen' specialized to 'IO'.
type GenIO = Gen RealWorld

-- | Type alias of 'Gen' specialized to 'ST'. (
type GenST s = Gen s
-- Note this doesn't force it to be in ST. You can write (STGen Realworld)
-- and it'll work in IO. Writing STGen s = Gen (PrimState (ST s)) doesn't
-- solve this.

-- | Create a 'Gen' from a fixed initial 'seed'.
create :: PrimMonad m => m (Gen (PrimState m))
create = restore seed

-- | Initialize a generator with two words.
--
--   >>> initialize 0 0 >>= save
--   FrozenGen 6364136223846793006 1
initialize :: PrimMonad m => Word64 -> Word64 -> m (Gen (PrimState m))
initialize a b = unsafePrimToPrim $ do
  p <- malloc
  pcg32_srandom_r p a b
  return (Gen p)

-- | Seed with system random number. (\"@\/dev\/urandom@\" on Unix-like
--   systems, time otherwise).
withSystemRandom :: PrimMonad m => (Gen (PrimState m) -> m a) -> IO a
withSystemRandom f = do
  w1 <- sysRandom
  w2 <- sysRandom
  unsafePrimToIO $ initialize w1 w2 >>= f

-- | Seed a PRNG with data from the system's fast source of pseudo-random
-- numbers. All the caveats of 'withSystemRandom' apply here as well.
createSystemRandom :: IO GenIO
createSystemRandom = withSystemRandom (return :: GenIO -> IO GenIO)

-- -- | Generate a uniform 'Word32' bounded by the given bound.
-- uniformB :: PrimMonad m => Word32 -> Gen (PrimState m) -> m Word32
-- uniformB u (Gen p) = unsafePrimToPrim $ pcg32_boundedrand_r p u
-- {-# INLINE uniformB #-}

-- | Advance the given generator n steps in log(n) time. (Note that a
--   \"step\" is a single random 32-bit (or less) 'Variate'. Data types
--   such as 'Double' or 'Word64' require two \"steps\".)
--
--   >>> create >>= \g -> replicateM_ 1000 (uniformW32 g) >> uniformW32 g
--   3640764222
--   >>> create >>= \g -> replicateM_ 500 (uniformD g) >> uniformW32 g
--   3640764222
--   >>> create >>= \g -> advance 1000 g >> uniformW32 g
--   3640764222
advance :: PrimMonad m => Word64 -> Gen (PrimState m) -> m ()
advance u (Gen p) = unsafePrimToPrim $ pcg32_advance_r p u
{-# INLINE advance #-}

-- | Retract the given generator n steps in log(2^64-n) time. This
--   is just @advance (-n)@.
--
--   >>> create >>= \g -> replicateM 3 (uniformW32 g)
--   [355248013,41705475,3406281715]
--   >>> create >>= \g -> retract 1 g >> replicateM 3 (uniformW32 g)
--   [19683962,355248013,41705475]
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

foreign import ccall unsafe "pcg_setseq_64_xsh_rr_32_boundedrand_r"
  pcg32_boundedrand_r :: Ptr FrozenGen -> Word32 -> IO Word32

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

  uniform1B f b (Gen p) = unsafePrimToPrim $ f <$> pcg32_boundedrand_r p b
  {-# INLINE uniform1B #-}

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

  split s = unsafeDupablePerformIO $ do
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

