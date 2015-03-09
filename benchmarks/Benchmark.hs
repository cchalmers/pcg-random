import Control.Exception
import Criterion.Main
import Data.Word

import qualified System.Random as R
import qualified System.Random.MWC as MWC
import qualified System.Random.PCG as PCG
import qualified System.Random.PCG.Fast as F
import qualified System.Random.PCG.Fast.Pure as PF
import qualified System.Random.PCG.Single as S
import qualified System.Random.PCG.Unique as U
import           System.Random.PCG.Pure
import           System.Random.PCG.Class
import qualified System.Random.Mersenne as M

benchIO :: String -> IO a -> Benchmark
benchIO s a = bench s (whnfIO a)
{-# INLINE benchIO #-}

main :: IO ()
main = do
  mwc  <- MWC.create
  pcg  <- PCG.create
  pcgF <- F.create
  pcgS <- S.create
  pcgU <- U.create
  pcgP <- makeState s0
  pcgFP <- PF.create
  mtg  <- M.newMTGen . Just =<< MWC.uniform mwc
  defaultMain
    -- [ bgroup "pcg"
    --   [ benchIO "Word32" (PCG.uniform pcg :: IO Word32)
    --   ]
    [ bgroup "pcg-fast"
      [ benchIO "Word32" (F.uniform pcgF :: IO Word32)
      , benchIO "Word32B" (F.uniformB 100 pcgF :: IO Word32)
    --   , benchIO "Word32R" (F.uniformR (0,10000) pcgF :: IO Word32)
    --   , benchIO "Word32B" (F.uniformB 10000 pcgF :: IO Word32)
    --   , benchIO "IntBViaFloat" (viaFloat 10000 pcgF :: IO Int)
    --   , benchIO "uniformFD" (uniformFD pcgF :: IO Double)
      ]
    -- , bgroup "pcg-pure"
    --   [ benchIO "Word32" (single' pcgP :: IO Word32)
    --   ]
    , bgroup "pcg-fast-pure"
      [ benchIO "Word32" (PF.uniform pcgFP :: IO Word32)
      , benchIO "Word32B" (PF.uniformB maxBound pcgFP :: IO Word32)
      -- , benchIO "Word32R" (F.uniformR (0,10000) pcgF :: IO Word32)
      -- , benchIO "Word32B" (F.uniformB 10000 pcgF :: IO Word32)
      -- , benchIO "IntBViaFloat" (viaFloat 10000 pcgF :: IO Int)
      -- , benchIO "uniformFD" (uniformFD pcgF :: IO Double)
      ]
    -- , bgroup "pcg-single"
    --   [ benchIO "Word32" (S.uniform pcgS :: IO Word32)
    --   ]
    -- , bgroup "pcg-unique"
    --   [ benchIO "Word32" (U.uniform pcgU :: IO Word32)
    --   ]
    -- , bgroup "mwc"
    --   [ benchIO "Word64" (MWC.uniform mwc :: IO Word64)
    --   , benchIO "Word32R" (MWC.uniformR (0,10000) mwc :: IO Word32)
    --   , benchIO "Double" (MWC.uniform mwc :: IO Double)
    --   -- , bench "Int"    (uniform mwc :: IO Int)
    --   ]
    -- , bgroup "random"
    --   [ benchIO "Word32" (R.randomIO >>= evaluate :: IO Word32)
    --   , benchIO "Double" (R.randomIO >>= evaluate :: IO Double)
    --   , benchIO "Int"    (R.randomIO >>= evaluate :: IO Int)
    --   ]
    -- , bgroup "mersenne"
    --   [ benchIO "Word64" (M.random mtg :: IO Word64)
    --   , benchIO "Double" (M.random mtg :: IO Double)
    --   -- , bench "Int" (M.random mtg :: IO Int)
    --   ]
    ]

viaFloat :: Int -> F.GenIO -> IO Int
viaFloat b g = do
  let b' = fromIntegral b
  x <- F.uniformF g
  return $! floor (x * b')
