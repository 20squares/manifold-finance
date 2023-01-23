module Main
  (main) where

import Analytics
import Parameterization

main :: IO ()
main = do
  let (strategyComplete,strategyAccept,strategyPublish) = testStrategyTupleTarget
  putStrLn "\n COMPLETE GAME"
  printOutputCompleteGame strategyComplete parameters
  putStrLn "\n ACCEPT SUBGAME"
  printOutputAcceptSubgame strategyAccept parameters
  putStrLn "\n PUBLISH SUBGAME"
  printOutputPublishSubgame strategyPublish parameters


