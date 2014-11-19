import Control.Exception
import Criterion.Main
import Data.Word

import qualified System.Random as R
import qualified System.Random.MWC as MWC
import qualified System.Random.PCG as PCG
import qualified System.Random.Mersenne as M

benchIO :: String -> IO a -> Benchmark
benchIO s a = bench s (whnfIO a)

main :: IO ()
main = do
  mwc <- MWC.create
  pcg <- PCG.create
  mtg <- M.newMTGen . Just =<< MWC.uniform mwc
  defaultMain
    [ bgroup "pcg"
      [ benchIO "Word32" (PCG.uniform pcg :: IO Word32)
      ]
    , bgroup "mwc"
      [ benchIO "Word32" (MWC.uniform mwc :: IO Word32)
      -- , bench "Double" (uniform mwc :: IO Double)
      -- , bench "Int"    (uniform mwc :: IO Int)
      ]
    -- , bgroup "random"
    --   [ benchIO "Word32" (R.randomIO >>= evaluate :: IO Word32)
    --   , bench "Double" (R.randomIO >>= evaluate :: IO Double)
    --   , bench "Int"    (R.randomIO >>= evaluate :: IO Int)
    --   ]
    , bgroup "mersenne"
      [ benchIO "Word32" (M.random mtg :: IO Word32)
      -- , bench "Double" (M.random mtg :: IO Double)
      -- , bench "Int" (M.random mtg :: IO Int)
      ]
    ]
