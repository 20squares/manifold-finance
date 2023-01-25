module Main
  (main) where

import Analytics
import Parameterization

main :: IO ()
main = do
  putStrLn "EVALUATION OF GAME"
  let (strategyComplete,strategyAccept,strategyPublish) = testStrategyTupleTarget
  putStrLn "\n COMPLETE GAME"
  printEquilibriumCompleteGame strategyComplete parameters
  putStrLn "\n ACCEPT SUBGAME"
  printEquilibriumAcceptSubgame strategyAccept parameters
  putStrLn "\n PUBLISH SUBGAME"
  printEquilibriumPublishSubgame strategyPublish parameters


