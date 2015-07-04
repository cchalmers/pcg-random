{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MagicHash               #-}
{-# LANGUAGE UnboxedTuples               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE RoleAnnotations            #-}
#endif
-- |
-- Module     : System.Random.PCG.Fast
-- Copyright  : Copyright (c) 2015, Christopher Chalmers <c.chalmers@me.com>
-- License    : BSD3
-- Maintainer : Christopher Chalmers <c.chalmers@me.com>
-- Stability  : experimental
-- Portability: CPP
--
-- Fast variant of the PCG random number generator. This module performs
-- around 20% faster than the multiple streams version but produces slightly
-- lower quality (still good) random numbers.
--
-- See <http://www.pcg-random.org> for details.
--
-- @
-- import Control.Monad.ST
-- import System.Random.PCG.Fast
--
-- three :: [Double]
-- three = runST $ do
--   g <- create
--   a <- uniform g
--   b <- uniform g
--   c <- uniform g
--   return [a,b,c]
-- @
module System.Random.PCG.Pure where
  -- ( -- * Gen
  --   Gen, GenIO, GenST
  -- , create, createSystemRandom, initialize, withSystemRandom

  --   -- * Getting random numbers
  -- , Variate (..)
  -- , advance, retract

  --   -- * Seeds
  -- , FrozenGen, save, restore, seed, initFrozen

  --   -- * Type restricted versions
  --   -- ** uniform
  -- , uniformW8, uniformW16, uniformW32, uniformW64
  -- , uniformI8, uniformI16, uniformI32, uniformI64
  -- , uniformF, uniformD, uniformBool

  --   -- ** uniformR
  -- , uniformRW8, uniformRW16, uniformRW32, uniformRW64
  -- , uniformRI8, uniformRI16, uniformRI32, uniformRI64
  -- , uniformRF, uniformRD, uniformRBool

  --   -- ** uniformB
  -- , uniformBW8, uniformBW16, uniformBW32, uniformBW64
  -- , uniformBI8, uniformBI16, uniformBI32, uniformBI64
  -- , uniformBF, uniformBD, uniformBBool
  -- ) where

-- import Control.Monad.Primitive
import Data.Bits
import GHC.Word
-- import GHC.Base
-- import Data.Functor
-- import Data.Primitive.ByteArray

-- import System.Random.PCG.Generic

-- $setup
-- >>> import System.Random.PCG.Fast
-- >>> import System.Random.PCG.Class
-- >>> import Control.Monad

data SetSeq = SetSeq {-# UNPACK #-} !Word64 -- step
                     {-# UNPACK #-} !Word64 -- sequence
  deriving (Show, Ord, Eq)

seed :: SetSeq
seed = SetSeq 9600629759793949339 15726070495360670683

-- internals -----------------------------------------------------------

-- All operations are done via Pair to ensure everything's strict. Ghc
-- is normally pretty good at inlining this, so Pair rarely exists in
-- core.
data Pair = Pair {-# UNPACK #-} !Word64 -- step
                 {-# UNPACK #-} !Word32 -- output

step :: SetSeq -> Word64
step (SetSeq s inc) = s * 6364136223846793005 + inc
{-# INLINE step #-}

output :: Word64 -> Word32
output s =
  (shifted `unsafeShiftR` rot) .|. (shifted `unsafeShiftL` (negate rot .&. 31))
  where
    rot     = fromIntegral $ s `shiftR` 59 :: Int
    shifted = fromIntegral $ ((s `shiftR` 18) `xor` s) `shiftR` 27 :: Word32
{-# INLINE output #-}

pair :: SetSeq -> Pair
pair g@(SetSeq s _) = Pair (step g) (output s)
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
{- INLINE bounded #-}

next :: SetSeq -> (Word32, SetSeq)
next g@(SetSeq _ inc) = (r, SetSeq s' inc)
  where Pair s' r = pair g

-- instance PCG SetSeq where
--   type Result SetSeq = Word32
--   type State SetSeq  = Word64
--   seed = SetSeq 0x853c49e6748fea9b 0xda3e39cb94b95bdb
--   initialize x = start x 0xda3e39cb94b95bdb
--   next g@(SetSeq _ inc) = g' `seq` (r, g')
--     where Pair s' r = pair g
--           g'        = SetSeq s' inc
--   split g@(SetSeq _ inc) = g1 `seq` g2 `seq` (g1, g2)
--     where
--       g1 = start32 w1 w7 w2 w3 -- mix it up
--       g2 = start32 w8 w6 w5 w4
--       Pair s1 w1 = pair g
--       Pair s2 w2 = pair (SetSeq s1 inc)
--       Pair s3 w3 = pair (SetSeq s2 inc)
--       Pair s4 w4 = pair (SetSeq s3 inc)
--       Pair s5 w5 = pair (SetSeq s4 inc)
--       Pair s6 w6 = pair (SetSeq s5 inc)
--       Pair s7 w7 = pair (SetSeq s6 inc)
--       w8 = output s7 -- abandon old state
--   {-# NOINLINE [1] list #-}
--   list (SetSeq s0 inc) = go s0
--     where go s = r : go s'
--             where Pair s' r = pair (SetSeq s inc)
--   bounded b (SetSeq s0 inc) = (rn, SetSeq sn inc)
--     where
--       Pair sn rn = go s0
--       t = negate b `mod` b
--       go s | r >= t    = Pair s' (r `mod` b)
--            | otherwise = go s'
--         where Pair s' r = pair (SetSeq s inc)
--   {- INLINE bounded #-}

listFB :: (Word32 -> b -> b) -> SetSeq -> b
listFB c (SetSeq s0 inc) = go s0
  where
    go s = r `c` go s'
      where
        Pair s' r = pair (SetSeq s inc)
{-# NOINLINE [0] listFB #-}


-- {-# RULES
-- "next"    [~1] forall s. list s     = build (\c _n -> listFB c s)
-- "nextFB"  [1]            listFB (:) = list
 -- #-}

-- start32 :: Word32 -> Word32 -> Word32 -> Word32 -> SetSeq
-- start32 a b c d = start (wordsTo64Bit a b) (wordsTo64Bit c d)

start :: Word64 -> Word64 -> SetSeq
start a b = SetSeq s i
  where
    s = step (SetSeq (a + i) i)
    i = (b `shiftL` 1) .|. 1

-- data MSetSeq s = MSetSeq {-# UNPACK #-} !(MutableByteArray s) -- step
--                          {-# UNPACK #-} !Word64               -- sequence

-- uniform1 :: PrimMonad m => (Word32 -> a) -> MSetSeq (PrimState m) -> m a
-- uniform1 f (MSetSeq a inc) = do
--   s <- readByteArray a 0
--   let Pair a s' = pair (SetSeq s inc)
--   writeByteArray a 0 s'
--   return $! f a

-- uniform2 :: PrimMonad m => (Word32 -> Word32 -> a) -> MSetSeq (PrimState m) -> m a
-- uniform2 f (MSetSeq a inc) = do
--   s <- readByteArray a 0
--   let Pair a s'  = pair (SetSeq s inc)
--       Pair b s'' = pair (SetSeq s' inc)
--   writeByteArray a 0 s''
--   return $! f a b

-- uniformB :: PrimMonad m => (Word32 -> a) -> b -> MSetSeq (PrimState m) -> m a
-- uniformB f (MSetSeq a inc) = do
--   s <- readByteArray a 0
--   let Pair a s'  = bounded b (SetSeq s inc)
--   writeByteArray a 0 s''
--   return $! f a

