{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Analytics
  where

import OpenGames.Engine.Engine

import Diagnostics
import Model
import Payoffs
import Types 

{-
Defines and runs the main analytics for the model
-}

------------------------
-- 1. Equilibrium notion
equilibriumCompleteGame strategy Parameters{..} = evaluate (completeGame buyerName sellerName distribution actionSpaceGasPub) strategy context
  where
    context =
      StochasticStatefulContext
         (pure ((),(transaction,contract,piInitial))) (\_ _ -> pure ())


printOutput strategy parameters = do
  let buyer1 ::- buyer2 ::- seller3 ::- buyer4 ::- buyer5 ::- buyer6 ::- seller7 ::- seller8 ::- Nil = equilibriumCompleteGame strategy parameters
  putStrLn "Initial Decision by Buyer:"
  putStrLn $ checkEqL buyer1
  putStrLn "NoLH Publish Decision by Buyer:"
  putStrLn $ checkEqMaybeL buyer2
  putStrLn "Accept Decision by Seller:"
  putStrLn $ checkEqMaybeL seller3
  putStrLn "Recoup Decision by Buyer:"
  putStrLn $ checkEqMaybe2L buyer4
  putStrLn "LH Publish Decision by Buyer:"
  putStrLn $ checkEqMaybe2L buyer5
  putStrLn "LH Publish Decision by Buyer - gasPub:"
  putStrLn $ checkEqMaybe2L buyer6
  putStrLn "Fulfill Decision by Seller:"
  putStrLn $ checkEqMaybe3L seller7
  putStrLn "NoFulfill Decision by Seller:"
  putStrLn $ checkEqMaybe3L seller8


