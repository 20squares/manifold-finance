{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Main
  (main) where

import OpenGames.Engine.Engine

import Analytics
import Parameterization
import Types 

import qualified Data.ByteString.Lazy as L
import           Data.Csv
import qualified Data.Vector          as V
import           Path
import qualified Path.IO as IO

-- 1. import individual prob distribution
-- | source files
dirProbability = [reldir|probability|]
distributionFile = [relfile|distribution.csv|]

  
-- | File
importProbDistFile
  :: FilePath
     -> IO (Either
              String
              (Stochastic GasPrice))
importProbDistFile filePath = do 
  content <- L.readFile filePath
  let decodedContent = decodeImportProb content
  case decodedContent of
    Left x -> pure $ Left x
    Right (_,vector) -> pure $ Right $ fromVectorToProbDist vector



  
--importFile = do
--  scenario <- importProbDistFile $ toFilePath $ dirSpecificationSimulation </> specificationFile
-- 2. main executable
main :: IO ()
main = do
  distribution <- importProbDistFile $ toFilePath $ dirProbability </> distributionFile
  case distribution of
    Left x -> print x
    Right probDist ->  do
        putStrLn "EVALUATION OF GAME"
        let (strategyComplete,strategyAccept,strategyPublish) = testStrategyTupleTarget
        putStrLn "\n COMPLETE GAME"
        printEquilibriumCompleteGame strategyComplete (parameters probDist)
        putStrLn "\n ACCEPT SUBGAME"
        printEquilibriumAcceptSubgame strategyAccept (parameters probDist)
        putStrLn "\n PUBLISH SUBGAME"
        printEquilibriumPublishSubgame strategyPublish (parameters probDist)


