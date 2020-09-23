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
-- Module     : System.Random.PCG.Pure
-- Copyright  : Copyright (c) 2015, Christopher Chalmers <c.chalmers@me.com>
-- License    : BSD3
-- Maintainer : Christopher Chalmers <c.chalmers@me.com>
-- Stability  : experimental
-- Portability: CPP
--
-- Standard PCG Random Number Generator with chosen streams, written in
-- pure haskell. See <http://www.pcg-random.org> for details.
--
-- @
-- import Control.Monad.ST
-- import System.Random.PCG.Pure
--
-- three :: [Double]
-- three = runST $ do
--   g <- create
--   a <- uniform g
--   b <- uniform g
--   c <- uniform g
--   return [a,b,c]
-- @
module System.Random.PCG.Pure
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

    -- * Pure
  , SetSeq
  , next'
  , advanceSetSeq
  ) where


import Control.Monad.Primitive
import Data.Bits
import Data.Data
import Data.Primitive.ByteArray
import Foreign
import GHC.Generics

import System.Random.PCG.Class
import System.Random

-- $setup
-- >>> import System.Random.PCG.Pure as Pure
-- >>> import Control.Monad

type GenIO = Gen RealWorld
type GenST = Gen

-- | State of the random number generator
newtype Gen s = G (MutableByteArray s)

type FrozenGen = SetSeq

-- | The multiple sequence varient of the pcg random number generator.
data SetSeq = SetSeq
  {-# UNPACK #-} !Word64 -- step
  {-# UNPACK #-} !Word64 -- sequence
  deriving (Show, Ord, Eq, Data, Typeable, Generic)

instance Storable SetSeq where
  sizeOf _ = 16
  {-# INLINE sizeOf #-}
  alignment _ = 8
  {-# INLINE alignment #-}
  poke ptr (SetSeq x y) = poke ptr' x >> pokeElemOff ptr' 1 y
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = do
    let ptr' = castPtr ptr
    s <- peek ptr'
    inc <- peekElemOff ptr' 1
    return $ SetSeq s inc
  {-# INLINE peek #-}

-- | Fixed seed.
seed :: SetSeq
seed = SetSeq 9600629759793949339 15726070495360670683

-- Internals -----------------------------------------------------------

-- All operations are done via Pair to ensure everything's strict. Ghc
-- is normally pretty good at inlining this, so Pair rarely exists in
-- core.
data Pair = Pair
  {-# UNPACK #-} !Word64 -- step
  {-# UNPACK #-} !Word32 -- output

multiplier :: Word64
multiplier = 6364136223846793005

-- A single step in the generator
state :: SetSeq -> Word64
state (SetSeq s inc) = s * multiplier + inc
{-# INLINE state #-}

-- The random number output
output :: Word64 -> Word32
output s =
  (shifted `unsafeShiftR` rot) .|. (shifted `unsafeShiftL` (negate rot .&. 31))
  where
    rot     = fromIntegral $ s `shiftR` 59 :: Int
    shifted = fromIntegral $ ((s `shiftR` 18) `xor` s) `shiftR` 27 :: Word32
{-# INLINE output #-}

-- increment the sequence by one
pair :: SetSeq -> Pair
pair g@(SetSeq s _) = Pair (state g) (output s)
{-# INLINE pair #-}

-- Given some bound and the generator, compute the new step and bounded
-- random number.
bounded :: Word32 -> SetSeq -> Pair
bounded b (SetSeq s0 inc) = go s0
  where
    t = negate b `mod` b
    go !s | r >= t    = Pair s' (r `mod` b)
          | otherwise = go s'
      where Pair s' r = pair (SetSeq s inc)
{-# INLINE bounded #-}

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

-- | Pure version of 'advance'.
advanceSetSeq :: Word64 -> FrozenGen -> FrozenGen
advanceSetSeq d (SetSeq s inc) = SetSeq (advancing d s multiplier inc) inc

advanceSetSeq' :: Word64 -> FrozenGen -> Word64
advanceSetSeq' d (SetSeq s inc) = advancing d s multiplier inc

-- | Create a new generator from two words.
start :: Word64 -> Word64 -> SetSeq
start a b = SetSeq s i
  where
    s = state (SetSeq (a + i) i)
    i = (b `shiftL` 1) .|. 1
{-# INLINE start #-}

-- | Version of 'next' that returns a 'Word32'.
next' :: SetSeq -> (Word32, SetSeq)
next' g@(SetSeq _ inc) = (r, SetSeq s' inc)
  where Pair s' r = pair g
{-# INLINE next' #-}

-- Multable ------------------------------------------------------------

-- | Save the state of a 'Gen' in a 'Seed'.
save :: PrimMonad m => Gen (PrimState m) -> m SetSeq
save (G a) = do
  s   <- readByteArray a 0
  inc <- readByteArray a 1
  return $ SetSeq s inc
{-# INLINE save #-}

-- | Restore a 'Gen' from a 'Seed'.
restore :: PrimMonad m => FrozenGen -> m (Gen (PrimState m))
restore (SetSeq s inc) = do
  a <- newByteArray 16
  writeByteArray a 0 s
  writeByteArray a 1 inc
  return $! G a
{-# INLINE restore #-}

-- | Create a new generator from two words.
--
-- >>> Pure.initFrozen 0 0
-- SetSeq 6364136223846793006 1
initFrozen :: Word64 -> Word64 -> SetSeq
initFrozen = start

-- | Create a 'Gen' from a fixed initial seed.
create :: PrimMonad m => m (Gen (PrimState m))
create = restore seed

-- | Initialize a generator a single word.
--
--   >>> Pure.initialize 0 0 >>= Pure.save
--   SetSeq 6364136223846793006 1
initialize :: PrimMonad m => Word64 -> Word64 -> m (Gen (PrimState m))
initialize a b = restore (initFrozen a b)

-- | Seed with system random number. (@\/dev\/urandom@ on Unix-like
--   systems and CryptAPI on Windows).
withSystemRandom :: (GenIO -> IO a) -> IO a
withSystemRandom f = do
  a <- sysRandom
  b <- sysRandom
  initialize a b >>= f

-- | Seed with system random number. (@\/dev\/urandom@ on Unix-like
--   systems and CryptAPI on Windows).
createSystemRandom :: IO GenIO
createSystemRandom = withSystemRandom return

-- | Advance the given generator n steps in log(n) time. (Note that a
--   \"step\" is a single random 32-bit (or less) 'Variate'. Data types
--   such as 'Double' or 'Word64' require two \"steps\".)
--
--   >>> Pure.create >>= \g -> replicateM_ 1000 (uniformW32 g) >> uniformW32 g
--   3640764222
--   >>> Pure.create >>= \g -> replicateM_ 500 (uniformD g) >> uniformW32 g
--   3640764222
--   >>> Pure.create >>= \g -> Pure.advance 1000 g >> uniformW32 g
--   3640764222
advance :: PrimMonad m => Word64 -> Gen (PrimState m) -> m ()
advance u g@(G a) = do
  ss <- save g
  let s' = advanceSetSeq' u ss
  writeByteArray a 0 s'
{-# INLINE advance #-}

-- | Retract the given generator n steps in log(2^64-n) time. This
--   is just @advance (-n)@.
--
--   >>> Pure.create >>= \g -> replicateM 3 (uniformW32 g)
--   [355248013,41705475,3406281715]
--   >>> Pure.create >>= \g -> Pure.retract 1 g >> replicateM 3 (uniformW32 g)
--   [19683962,355248013,41705475]
retract :: PrimMonad m => Word64 -> Gen (PrimState m) -> m ()
retract u g = advance (-u) g
{-# INLINE retract #-}

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance (PrimMonad m, s ~ PrimState m) => Generator (Gen s) m where
  uniform1 f (G a) = do
    s   <- readByteArray a 0
    inc <- readByteArray a 1
    writeByteArray a 0 $! s * multiplier + inc
    return $! f (output s)
  {-# INLINE uniform1 #-}

  uniform2 f (G a) = do
    s   <- readByteArray a 0
    inc <- readByteArray a 1
    let !s' = s * multiplier + inc
    writeByteArray a 0 $! s' * multiplier + inc
    return $! f (output s) (output s')
  {-# INLINE uniform2 #-}

  uniform1B f b g@(G a) = do
    ss <- save g
    let Pair s' r = bounded b ss
    writeByteArray a 0 s'
    return $! f r
  {-# INLINE uniform1B #-}

instance RandomGen FrozenGen where
  next (SetSeq s inc) = (wordsTo64Bit w1 w2, SetSeq s'' inc)
    where
      Pair s'  w1 = pair (SetSeq s inc)
      Pair s'' w2 = pair (SetSeq s' inc)
  {-# INLINE next #-}

  split (SetSeq s inc) = (SetSeq s4 inc, mk w1 w2 w3 w4)
    where
      mk a b c d = start (wordsTo64Bit a b) (wordsTo64Bit c d)
      Pair s1 w1 = pair (SetSeq s  inc)
      Pair s2 w2 = pair (SetSeq s1 inc)
      Pair s3 w3 = pair (SetSeq s2 inc)
      Pair s4 w4 = pair (SetSeq s3 inc)
  {-# INLINE split #-}
