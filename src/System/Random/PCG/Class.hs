{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Random.PCG.Class
  ( -- * Classes
    Generator (..)
  , Variate (..)

    -- * Type contricted versions
  , uniformW8, uniformW16, uniformW32, uniformW64
  , uniformI8, uniformI16, uniformI32, uniformI64
  , uniformF, uniformD

   -- * Utilities
  , Unsigned
  , wordsTo64Bit
  , wordToBool
  , wordToFloat
  , wordsToDouble
  , sysRandom
  ) where

import Data.Word
import Data.Int
import Data.Bits
import Control.Monad
import System.IO
import Foreign (allocaBytes, peek)

import Data.Ratio              ((%), numerator)
import Data.IORef              (atomicModifyIORef, newIORef)
import Data.Time.Clock.POSIX   (getPOSIXTime)
import System.CPUTime   (cpuTimePrecision, getCPUTime)
import qualified Control.Exception as E
import System.IO.Unsafe (unsafePerformIO)


class Monad m => Generator g m where
  uniform1 :: (Word32 -> a) -> g -> m a
  uniform2 :: (Word32 -> Word32 -> a) -> g -> m a

class Variate a where
  uniform  :: Generator g m => g -> m a
  uniformR :: Generator g m => (a,a) -> g -> m a
  uniformB :: (Generator g m, Integral a) => a -> g -> m a
  uniformB a g = uniformR (0,a) g
  {-# INLINE uniformB #-}

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

sub :: (Integral a, Integral (Unsigned a)) => a -> a -> Unsigned a
sub x y = fromIntegral x - fromIntegral y
{-# INLINE sub #-}

add :: (Integral a, Integral (Unsigned a)) => a -> Unsigned a -> a
add m x = m + fromIntegral x
{-# INLINE add #-}

instance Variate Int8 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}

uniformI8 :: Generator g m => g -> m Int8
uniformI8 = uniform
{-# INLINE uniformI8 #-}

instance Variate Int16 where
  uniform  = uniform1 fromIntegral
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}

uniformI16 :: Generator g m => g -> m Int16
uniformI16 = uniform
{-# INLINE uniformI16 #-}

instance Variate Int32 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}

uniformI32 :: Generator g m => g -> m Int32
uniformI32 = uniform
{-# INLINE uniformI32 #-}

instance Variate Int64 where
  uniform = uniform2 wordsTo64Bit
  uniformR a g = uniformRange a g
  {-# INLINE uniform  #-}
  {-# INLINE uniformR #-}

uniformI64 :: Generator g m => g -> m Int64
uniformI64 = uniform
{-# INLINE uniformI64 #-}

instance Variate Word8 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}

uniformW8 :: Generator g m => g -> m Word8
uniformW8 = uniform
{-# INLINE uniformW8 #-}

instance Variate Word16 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}

uniformW16 :: Generator g m => g -> m Word16
uniformW16 = uniform
{-# INLINE uniformW16 #-}

instance Variate Word32 where
  uniform = uniform1 fromIntegral
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}

uniformW32 :: Generator g m => g -> m Word32
uniformW32 = uniform
{-# INLINE uniformW32 #-}

instance Variate Word64 where
  uniform = uniform2 wordsTo64Bit
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}

uniformW64 :: Generator g m => g -> m Word64
uniformW64 = uniform
{-# INLINE uniformW64 #-}

instance Variate Bool where
  uniform = uniform1 wordToBool
  {-# INLINE uniform  #-}
  uniformR (False,True)  g = uniform g
  uniformR (False,False) _ = return False
  uniformR (True,True)   _ = return True
  uniformR (True,False)  g = uniform g
  {-# INLINE uniformR #-}

instance Variate Float where
  uniform = uniform1 wordToFloat
  {-# INLINE uniform  #-}
  uniformR (x1,x2) = uniform1 (\w -> x1 + (x2-x1) * wordToFloat w)
  {-# INLINE uniformR #-}

uniformF :: Generator g m => g -> m Float
uniformF = uniform
{-# INLINE uniformF #-}

instance Variate Double where
  uniform = uniform2 wordsToDouble
  {-# INLINE uniform  #-}
  uniformR (x1,x2) = uniform2 (\w1 w2 -> x1 + (x2-x1) * wordsToDouble w1 w2)
  {-# INLINE uniformR #-}

uniformD :: Generator g m => g -> m Double
uniformD = uniform
{-# INLINE uniformD #-}

instance Variate Word where
#if WORD_SIZE_IN_BITS < 64
  uniform = uniform1 fromIntegral
#else
  uniform = uniform2 wordsTo64Bit
#endif
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}

instance Variate Int where
#if WORD_SIZE_IN_BITS < 64
  uniform = uniform1 fromIntegral
#else
  uniform = uniform2 wordsTo64Bit
#endif
  {-# INLINE uniform  #-}
  uniformR a g = uniformRange a g
  {-# INLINE uniformR #-}

instance (Variate a, Variate b) => Variate (a,b) where
  uniform g = (,) `liftM` uniform g `ap` uniform g
  {-# INLINE uniform  #-}
  uniformR ((x1,y1),(x2,y2)) g = (,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g
  {-# INLINE uniformR #-}

instance (Variate a, Variate b, Variate c) => Variate (a,b,c) where
  uniform g = (,,) `liftM` uniform g `ap` uniform g `ap` uniform g
  {-# INLINE uniform  #-}
  uniformR ((x1,y1,z1),(x2,y2,z2)) g =
    (,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap` uniformR (z1,z2) g
  {-# INLINE uniformR #-}

instance (Variate a, Variate b, Variate c, Variate d) => Variate (a,b,c,d) where
  uniform g = (,,,) `liftM` uniform g `ap` uniform g `ap` uniform g `ap` uniform g
  {-# INLINE uniform  #-}
  uniformR ((x1,y1,z1,t1),(x2,y2,z2,t2)) g =
    (,,,) `liftM` uniformR (x1,x2) g `ap` uniformR (y1,y2) g `ap`
                  uniformR (z1,z2) g `ap` uniformR (t1,t2) g
  {-# INLINE uniformR #-}

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

-- Aquire seed from current time. This is horrible fallback for
-- Windows system.
acquireSeedTime :: IO Word64
acquireSeedTime = do
  c <- (numerator . (%cpuTimePrecision)) `liftM` getCPUTime
  t <- toRational `liftM` getPOSIXTime
  let n    = fromIntegral (numerator t) :: Word64
  return $ wordsTo64Bit (fromIntegral c) (fromIntegral n)

-- | Get a random number from system source. If "/dev/urandom" is not
--   found return inferior random number from time.
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

