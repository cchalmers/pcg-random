{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
-- |
-- Module     : System.Random.PCG.Class
-- Copyright  : Copyright (c) 2014-2015, Christopher Chalmers <c.chalmers@me.com>
-- License    : BSD3
-- Maintainer : Christopher Chalmers <c.chalmers@me.com>
-- Stability  : experimental
-- Portability: CPP
--
-- Classes for working with random numbers along with utility functions.
-- In a future release this module may disappear and use another module
-- for this functionality.
module System.Random.PCG.Class
  ( -- * Classes
    Generator (..)
  , Variate (..)

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

   -- * Utilities
  , Unsigned
  , wordsTo64Bit
  , wordToBool
  , wordToFloat
  , wordsToDouble
  , sysRandom
  ) where

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

import           Control.Monad
import           Data.Bits
import           Data.Int
import           Data.Word
import           Foreign               (allocaBytes, peek)
import           System.IO

import qualified Control.Exception     as E
import           Data.IORef            (atomicModifyIORef, newIORef)
import           Data.Ratio            (numerator, (%))
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.CPUTime        (cpuTimePrecision, getCPUTime)
import           System.IO.Unsafe      (unsafePerformIO)

class Monad m => Generator g m where
  uniform1 :: (Word32 -> a) -> g -> m a
  uniform2 :: (Word32 -> Word32 -> a) -> g -> m a
  uniform1B :: Integral a => (Word32 -> a) -> Word32 -> g -> m a

class Variate a where
  -- | Generate a uniformly distributed random vairate.
  --
  --   * Use entire range for integral types.
  --
  --   * Use (0,1] range for floating types.
  uniform  :: Generator g m => g -> m a

  -- | Generate a uniformly distributed random vairate in the given
  --   range.
  --
  --   * Use inclusive range for integral types.
  --
  --   * Use (a,b] range for floating types.
  uniformR :: Generator g m => (a,a) -> g -> m a

  -- | Generate a uniformly distributed random vairate in the range
  --   [0,b). For integral types the bound must be less than the max bound
  --   of 'Word32' (4294967295). Behaviour is undefined for negative
  --   bounds.
  uniformB :: Generator g m => a -> g -> m a

------------------------------------------------------------------------
-- Variate instances
------------------------------------------------------------------------

instance Variate Int8 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}
  uniformB b g = uniform1B fromIntegral (fromIntegral b) g
  {-# INLINE uniformB #-}

instance Variate Int16 where
  uniform  = uniform1 fromIntegral
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}
  uniformB b g = uniform1B fromIntegral (fromIntegral b) g
  {-# INLINE uniformB #-}

instance Variate Int32 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}
  uniformB b g = uniform1B fromIntegral (fromIntegral b) g
  {-# INLINE uniformB #-}

instance Variate Int64 where
  uniform = uniform2 wordsTo64Bit
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}
  uniformB b g = uniform1B fromIntegral (fromIntegral b) g
  {-# INLINE uniformB #-}

instance Variate Word8 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}
  uniformB b g = uniform1B fromIntegral (fromIntegral b) g
  {-# INLINE uniformB #-}

instance Variate Word16 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}
  uniformB b g = uniform1B fromIntegral (fromIntegral b) g
  {-# INLINE uniformB #-}

instance Variate Word32 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}
  uniformB b g = uniform1B fromIntegral (fromIntegral b) g
  {-# INLINE uniformB #-}

instance Variate Word64 where
  uniform = uniform2 wordsTo64Bit
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}
  uniformB b g = uniform1B fromIntegral (fromIntegral b) g
  {-# INLINE uniformB #-}

instance Variate Bool where
  uniform = uniform1 wordToBool
  {-# INLINE uniform  #-}
  uniformR (False,True)  g = uniform g
  uniformR (False,False) _ = return False
  uniformR (True,True)   _ = return True
  uniformR (True,False)  g = uniform g
  {-# INLINE uniformR #-}
  uniformB False _ = return False
  uniformB _     g = uniform g
  {-# INLINE uniformB #-}

instance Variate Float where
  uniform = uniform1 wordToFloat
  {-# INLINE uniform  #-}
  uniformR (x1,x2) = uniform1 (\w -> x1 + (x2-x1) * wordToFloat w)
  {-# INLINE uniformR #-}
  -- subtract 2**(-33) to go from (0,b] to [0,b) (I think)
  uniformB b g = (subtract 1.16415321826934814453125e-10) `liftM` uniformR (0,b) g
  {-# INLINE uniformB #-}

instance Variate Double where
  uniform = uniform2 wordsToDouble
  {-# INLINE uniform  #-}
  uniformR (x1,x2) = uniform2 (\w1 w2 -> x1 + (x2-x1) * wordsToDouble w1 w2)
  {-# INLINE uniformR #-}
  -- subtract 2**(-53) to go from (0,b] to [0,b) (I think)
  uniformB b g = (subtract 1.1102230246251565404236316680908203125e-16) `liftM` uniformR (0,b) g
  {-# INLINE uniformB #-}

instance Variate Word where
#if WORD_SIZE_IN_BITS < 64
  uniform = uniform1 fromIntegral
#else
  uniform = uniform2 wordsTo64Bit
#endif
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}
  uniformB b g = uniform1B fromIntegral (fromIntegral b) g
  {-# INLINE uniformB #-}

instance Variate Int where
#if WORD_SIZE_IN_BITS < 64
  uniform = uniform1 fromIntegral
#else
  uniform = uniform2 wordsTo64Bit
#endif
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}
  uniformB b g = uniform1B fromIntegral (fromIntegral b) g
  {-# INLINE uniformB #-}

instance (Variate a, Variate b) => Variate (a,b) where
  uniform g = (,) `liftM` uniform g `ap` uniform g
  {-# INLINE uniform  #-}
  uniformR ((x1,y1),(x2,y2)) g = (,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g
  {-# INLINE uniformR #-}
  uniformB (b1,b2) g = (,) `liftM` uniformB b1 g `ap` uniformB b2 g
  {-# INLINE uniformB #-}

instance (Variate a, Variate b, Variate c) => Variate (a,b,c) where
  uniform g = (,,) `liftM` uniform g `ap` uniform g `ap` uniform g
  {-# INLINE uniform  #-}
  uniformR ((x1,y1,z1),(x2,y2,z2)) g =
    (,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap` uniformR (z1,z2) g
  {-# INLINE uniformR #-}
  uniformB (b1,b2,b3) g = (,,) `liftM` uniformB b1 g `ap` uniformB b2 g `ap` uniformB b3 g
  {-# INLINE uniformB #-}

instance (Variate a, Variate b, Variate c, Variate d) => Variate (a,b,c,d) where
  uniform g = (,,,) `liftM` uniform g `ap` uniform g `ap` uniform g `ap` uniform g
  {-# INLINE uniform  #-}
  uniformR ((x1,y1,z1,t1),(x2,y2,z2,t2)) g =
    (,,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap`
                  uniformR (z1,z2) g `ap` uniformR (t1,t2) g
  {-# INLINE uniformR #-}
  uniformB (b1,b2,b3,b4) g = (,,,) `liftM` uniformB b1 g `ap` uniformB b2 g `ap` uniformB b3 g `ap` uniformB b4 g
  {-# INLINE uniformB #-}

------------------------------------------------------------------------
-- Type restricted versions
------------------------------------------------------------------------

-- uniform -------------------------------------------------------------

uniformI8 :: Generator g m => g -> m Int8
uniformI8 = uniform
{-# INLINE uniformI8 #-}

uniformI16 :: Generator g m => g -> m Int16
uniformI16 = uniform
{-# INLINE uniformI16 #-}

uniformI32 :: Generator g m => g -> m Int32
uniformI32 = uniform
{-# INLINE uniformI32 #-}

uniformI64 :: Generator g m => g -> m Int64
uniformI64 = uniform
{-# INLINE uniformI64 #-}

uniformW8 :: Generator g m => g -> m Word8
uniformW8 = uniform
{-# INLINE uniformW8 #-}

uniformW16 :: Generator g m => g -> m Word16
uniformW16 = uniform
{-# INLINE uniformW16 #-}

uniformW32 :: Generator g m => g -> m Word32
uniformW32 = uniform
{-# INLINE uniformW32 #-}

uniformW64 :: Generator g m => g -> m Word64
uniformW64 = uniform
{-# INLINE uniformW64 #-}

uniformBool :: Generator g m => g -> m Bool
uniformBool = uniform
{-# INLINE uniformBool #-}

uniformF :: Generator g m => g -> m Float
uniformF = uniform
{-# INLINE uniformF #-}

uniformD :: Generator g m => g -> m Double
uniformD = uniform
{-# INLINE uniformD #-}

-- uniformR ------------------------------------------------------------

uniformRI8 :: Generator g m => (Int8, Int8) -> g -> m Int8
uniformRI8 = uniformR
{-# INLINE uniformRI8 #-}

uniformRI16 :: Generator g m => (Int16, Int16) -> g -> m Int16
uniformRI16 = uniformR
{-# INLINE uniformRI16 #-}

uniformRI32 :: Generator g m => (Int32, Int32) -> g -> m Int32
uniformRI32 = uniformR
{-# INLINE uniformRI32 #-}

uniformRI64 :: Generator g m => (Int64, Int64) -> g -> m Int64
uniformRI64 = uniformR
{-# INLINE uniformRI64 #-}

uniformRW8 :: Generator g m => (Word8, Word8) -> g -> m Word8
uniformRW8 = uniformR
{-# INLINE uniformRW8 #-}

uniformRW16 :: Generator g m => (Word16, Word16) -> g -> m Word16
uniformRW16 = uniformR
{-# INLINE uniformRW16 #-}

uniformRW32 :: Generator g m => (Word32, Word32) -> g -> m Word32
uniformRW32 = uniformR
{-# INLINE uniformRW32 #-}

uniformRW64 :: Generator g m => (Word64, Word64) -> g -> m Word64
uniformRW64 = uniformR
{-# INLINE uniformRW64 #-}

uniformRBool :: Generator g m => (Bool, Bool) -> g -> m Bool
uniformRBool = uniformR
{-# INLINE uniformRBool #-}

uniformRF :: Generator g m => (Float, Float) -> g -> m Float
uniformRF = uniformR
{-# INLINE uniformRF #-}

uniformRD :: Generator g m => (Double, Double) -> g -> m Double
uniformRD = uniformR
{-# INLINE uniformRD #-}

-- uniformB ------------------------------------------------------------

uniformBI8 :: Generator g m => Int8 -> g -> m Int8
uniformBI8 = uniformB
{-# INLINE uniformBI8 #-}

uniformBI16 :: Generator g m => Int16 -> g -> m Int16
uniformBI16 = uniformB
{-# INLINE uniformBI16 #-}

uniformBI32 :: Generator g m => Int32 -> g -> m Int32
uniformBI32 = uniformB
{-# INLINE uniformBI32 #-}

uniformBI64 :: Generator g m => Int64 -> g -> m Int64
uniformBI64 = uniformB
{-# INLINE uniformBI64 #-}

uniformBW8 :: Generator g m => Word8 -> g -> m Word8
uniformBW8 = uniformB
{-# INLINE uniformBW8 #-}

uniformBW16 :: Generator g m => Word16 -> g -> m Word16
uniformBW16 = uniformB
{-# INLINE uniformBW16 #-}

uniformBW32 :: Generator g m => Word32 -> g -> m Word32
uniformBW32 = uniformB
{-# INLINE uniformBW32 #-}

uniformBW64 :: Generator g m => Word64 -> g -> m Word64
uniformBW64 = uniformB
{-# INLINE uniformBW64 #-}

uniformBBool :: Generator g m => Bool -> g -> m Bool
uniformBBool = uniformB
{-# INLINE uniformBBool #-}

uniformBF :: Generator g m => Float -> g -> m Float
uniformBF = uniformB
{-# INLINE uniformBF #-}

uniformBD :: Generator g m => Double -> g -> m Double
uniformBD = uniformB
{-# INLINE uniformBD #-}

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

sub :: (Integral a, Integral (Unsigned a)) => a -> a -> Unsigned a
sub x y = fromIntegral x - fromIntegral y
{-# INLINE sub #-}

add :: (Integral a, Integral (Unsigned a)) => a -> Unsigned a -> a
add m x = m + fromIntegral x
{-# INLINE add #-}

wordsTo64Bit :: Integral a => Word32 -> Word32 -> a
wordsTo64Bit x y =
    fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)
{-# INLINE wordsTo64Bit #-}

wordToBool :: Word32 -> Bool
wordToBool i = (i .&. 1) /= 0
{-# INLINE wordToBool #-}

wordToFloat :: Word32 -> Float
wordToFloat x      = (fromIntegral i * m_inv_32) + 0.5 + m_inv_33
    where m_inv_33 = 1.16415321826934814453125e-10
          m_inv_32 = 2.3283064365386962890625e-10
          i        = fromIntegral x :: Int32
{-# INLINE wordToFloat #-}

wordsToDouble :: Word32 -> Word32 -> Double
wordsToDouble x y  = (fromIntegral u * m_inv_32 + (0.5 + m_inv_53) +
                     fromIntegral (v .&. 0xFFFFF) * m_inv_52)
    where m_inv_52 = 2.220446049250313080847263336181640625e-16
          m_inv_53 = 1.1102230246251565404236316680908203125e-16
          m_inv_32 = 2.3283064365386962890625e-10
          u        = fromIntegral x :: Int32
          v        = fromIntegral y :: Int32
{-# INLINE wordsToDouble #-}

-- IO randoms

devRandom :: IO Word64
devRandom =
  allocaBytes 8 $ \buf -> do
    nread <- withBinaryFile "/dev/urandom" ReadMode $ \h -> hGetBuf h buf 8
    when (nread /= 8) $ error "unable to read from /dev/urandom"
    peek buf

-- Acquire seed from current time. This is horrible fall-back for
-- Windows system.
acquireSeedTime :: IO Word64
acquireSeedTime = do
  c <- (numerator . (%cpuTimePrecision)) `liftM` getCPUTime
  t <- toRational `liftM` getPOSIXTime
  let n    = fromIntegral (numerator t) :: Word64
  return $ wordsTo64Bit (fromIntegral c) (fromIntegral n)

-- | Get a random number from system source. If \"@\/dev\/urandom@\" is
--   not found return inferior random number from time.
sysRandom :: IO Word64
sysRandom =
  devRandom `E.catch` \(_ :: E.IOException) -> do
    seen <- atomicModifyIORef warned ((,) True)
    unless seen $ E.handle (\(_::E.IOException) -> return ()) $ do
      hPutStrLn stderr ("Warning: Couldn't open /dev/urandom")
      hPutStrLn stderr ("Warning: using system clock for seed instead " ++
                        "(quality will be lower)")
    acquireSeedTime
  where
    warned = unsafePerformIO $ newIORef False
    {-# NOINLINE warned #-}

uniformRange :: ( Generator g m
                , Integral a, Bounded a, Variate a
                , Integral (Unsigned a), Bounded (Unsigned a), Variate (Unsigned a))
             => (a,a) -> g -> m a
uniformRange (x1,x2) g
  | n == 0    = uniform g   -- Abuse overflow in unsigned types
  | otherwise = loop
  where
    -- Allow ranges where x2<x1
    (i, j) | x1 < x2   = (x1, x2)
           | otherwise = (x2, x1)
    -- (# i, j #) | x1 < x2   = (# x1, x2 #)
    --            | otherwise = (# x2, x1 #)
    n       = 1 + sub j i
    buckets = maxBound `div` n
    maxN    = buckets * n
    loop    = do x <- uniform g
                 if x < maxN then return $! add i (x `div` buckets)
                             else loop
{-# INLINE uniformRange #-}

-- Type family for fixed size integrals. For signed data types it's
-- its unsigned couterpart with same size and for unsigned data types
-- it's same type
type family Unsigned a :: *

type instance Unsigned Int8  = Word8
type instance Unsigned Int16 = Word16
type instance Unsigned Int32 = Word32
type instance Unsigned Int64 = Word64

type instance Unsigned Word8  = Word8
type instance Unsigned Word16 = Word16
type instance Unsigned Word32 = Word32
type instance Unsigned Word64 = Word64

-- GHC-7.6 has a bug (#8072) which results in calculation of wrong
-- number of buckets in function `uniformRange'. Consequently uniformR
-- generates values in wrong range.
#if (WORD_SIZE_IN_BITS < 64) && (__GLASGOW_HASKELL__ == 706)
type instance Unsigned Int   = Word32
type instance Unsigned Word  = Word32
#else
type instance Unsigned Int   = Word
type instance Unsigned Word  = Word
#endif
