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
-- distributionFile = [relfile|distributionOneElement.csv|]

-- | List of payments to check
lsPiContractToCheck = [90,91..110]

-- | List of exponential utility parameters to check
lsUtilityParameters = [1,1.1..2.0]

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

-- 2. main executable
main :: IO ()
main = do
  distribution <- importProbDistFile $ toFilePath $ dirProbability </> distributionFile
  case distribution of
    Left x -> print x
    Right probDist ->  do
        putStrLn "Evaluation of different payments with zero costs"
        let (strategyComplete,strategyAccept,strategyPublish) = testStrategyTupleTarget
            ls = fmap (\piContract -> (piContract, breakEquilibriumCompleteGame strategyComplete (parameters probDist piContract 0 0 0  2.0 2.0))) lsPiContractToCheck
        print ls
        putStrLn "Evaluation of risk parameters for exponential utility, costs as in the paper"
        let ls' = fmap (\utilityParameter -> (utilityParameter, breakEquilibriumCompleteGame strategyComplete (parameters probDist 100 (0.1*10**6) (75*10**3) (20*10**3) utilityParameter utilityParameter))) lsUtilityParameters
        print ls'

-- 3. main for the interactive version
interactiveMain :: Double -> PayoffHL -> PayoffHL -> IO ()
interactiveMain piContract utilityParameterBuyer utilityParameterSeller = do
  distribution <- importProbDistFile $ toFilePath $ dirProbability </> distributionFile
  case distribution of
    Left x -> print x
    Right probDist ->  do
        putStrLn "Evaluation of negative payments with zero costs"
        let (strategyComplete,strategyAccept,strategyPublish) = testStrategyTupleTarget
            ls = (piContract, utilityParameterBuyer, utilityParameterSeller, breakEquilibriumCompleteGame strategyComplete (parameters probDist piContract 0 0 0 utilityParameterBuyer utilityParameterSeller))
        print ls
        putStrLn "Evaluation of risk parameters with LH paper costs"                  
        let ls' = (piContract, utilityParameterBuyer, utilityParameterSeller, breakEquilibriumCompleteGame strategyComplete (parameters probDist piContract (0.1*10**6) (75*10**3) (20*10**3) utilityParameterBuyer utilityParameterSeller))
        print ls'
