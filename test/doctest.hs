import System.Environment (getEnvironment)
import Test.DocTest

main = do
  env <- getEnvironment
  let dist = case lookup "HASKELL_DIST_DIR" env of
               Nothing -> "dist"
               Just x -> x
  doctest [ "src/System/Random/PCG.hs"
          , "src/System/Random/PCG/Class.hs"
          , "src/System/Random/PCG/Pure.hs"
          , "src/System/Random/PCG/Fast.hs"
          , "src/System/Random/PCG/Single.hs"
          , dist ++ "/build/c/pcg-advance-64.o"
          , dist ++ "/build/c/pcg-output-64.o"
          , dist ++ "/build/c/pcg-rngs-64.o"
          ]
