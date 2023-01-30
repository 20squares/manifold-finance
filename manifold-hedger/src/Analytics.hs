{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Analytics
  where

import OpenGames.Engine.Engine

import Diagnostics
import Model
import Payoffs
import Types

{-
Defines the main analytics for the model
-}

------------------------
-- 1. Equilibrium notion
-- | Equilibrium definition for complete game
equilibriumCompleteGame strategy Parameters{..} = evaluate (completeGame buyerWealth sellerWealth distribution actionSpaceGasPub utilityFunctionBuyer utilityFunctionSeller) strategy context
  where
    context =
      StochasticStatefulContext
         (pure ((),(transaction,contract,piInitial))) (\_ _ -> pure ())

-- | Equilibrium definition for accept subgame
equilibriumAcceptSubGame strategy Parameters{..} = evaluate (acceptSubgame  buyerWealth sellerWealth distribution actionSpaceGasPub utilityFunctionBuyer utilityFunctionSeller) strategy context
  where
    context =
      StochasticStatefulContext
         (pure ((),(transaction,contract,piInitial))) (\_ _ -> pure ())

-- | Equilibrium definition for publish subgame
equilibriumPublishSubGame strategy Parameters{..} = evaluate (publishSubgame  buyerWealth sellerWealth distribution actionSpaceGasPub utilityFunctionBuyer utilityFunctionSeller) strategy context
  where
    context =
      StochasticStatefulContext
         (pure ((),(transaction,contract,piInitial))) (\_ _ -> pure ())

-- 2. Display equilibrium information only
-- | Hand-rolling the specific output type for complete game
printEquilibriumCompleteGame strategy parameters = do
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

-- | Hand-rolling the specific output type for accept subgame
printEquilibriumAcceptSubgame strategy parameters = do
  let seller3 ::- buyer4 ::- buyer5 ::- buyer6 ::- seller7 ::- seller8 ::- Nil = equilibriumAcceptSubGame strategy parameters
  putStrLn "Accept Decision by Seller:"
  putStrLn $ checkEqL seller3
  putStrLn "Recoup Decision by Buyer:"
  putStrLn $ checkEqMaybeL buyer4
  putStrLn "LH Publish Decision by Buyer:"
  putStrLn $ checkEqMaybeL buyer5
  putStrLn "LH Publish Decision by Buyer - gasPub:"
  putStrLn $ checkEqMaybeL buyer6
  putStrLn "Fulfill Decision by Seller:"
  putStrLn $ checkEqMaybe2L seller7
  putStrLn "NoFulfill Decision by Seller:"
  putStrLn $ checkEqMaybe2L seller8

-- | Hand-rolling the specific output type for publish subgame
printEquilibriumPublishSubgame strategy parameters = do
  let  buyer5 ::- buyer6 ::- seller7 ::- seller8 ::- Nil = equilibriumPublishSubGame strategy parameters
  putStrLn "LH Publish Decision by Buyer:"
  putStrLn $ checkEqL buyer5
  putStrLn "LH Publish Decision by Buyer - gasPub:"
  putStrLn $ checkEqL buyer6
  putStrLn "Fulfill Decision by Seller:"
  putStrLn $ checkEqMaybeL seller7
  putStrLn "NoFulfill Decision by Seller:"
  putStrLn $ checkEqMaybeL seller8


-- 3. Display full information
printOutputCompleteGame strategy parameters = do
  let buyer1 ::- buyer2 ::- seller3 ::- buyer4 ::- buyer5 ::- buyer6 ::- seller7 ::- seller8 ::- Nil = equilibriumCompleteGame strategy parameters
  putStrLn "Initial Decision by Buyer:"
  putStrLn $ showDiagnosticInfoL buyer1
  putStrLn "NoLH Publish Decision by Buyer:"
  putStrLn $ showDiagnosticInfoMaybeL buyer2
  putStrLn "Accept Decision by Seller:"
  putStrLn $ showDiagnosticInfoMaybeL seller3
  putStrLn "Recoup Decision by Buyer:"
  putStrLn $ showDiagnosticInfoMaybe2L buyer4
  putStrLn "LH Publish Decision by Buyer:"
  putStrLn $ showDiagnosticInfoMaybe2L buyer5
  putStrLn "LH Publish Decision by Buyer - gasPub:"
  putStrLn $ showDiagnosticInfoMaybe2L buyer6
  putStrLn "Fulfill Decision by Seller:"
  putStrLn $ showDiagnosticInfoMaybe3L seller7
  putStrLn "NoFulfill Decision by Seller:"
  putStrLn $ showDiagnosticInfoMaybe3L seller8

-- | Hand-rolling the specific output type for accept subgame
printOutputAcceptSubgame strategy parameters = do
  let seller3 ::- buyer4 ::- buyer5 ::- buyer6 ::- seller7 ::- seller8 ::- Nil = equilibriumAcceptSubGame strategy parameters
  putStrLn "Accept Decision by Seller:"
  putStrLn $ showDiagnosticInfoL seller3
  putStrLn "Recoup Decision by Buyer:"
  putStrLn $ showDiagnosticInfoMaybeL buyer4
  putStrLn "LH Publish Decision by Buyer:"
  putStrLn $ showDiagnosticInfoMaybeL buyer5
  putStrLn "LH Publish Decision by Buyer - gasPub:"
  putStrLn $ showDiagnosticInfoMaybeL buyer6
  putStrLn "Fulfill Decision by Seller:"
  putStrLn $ showDiagnosticInfoMaybe2L seller7
  putStrLn "NoFulfill Decision by Seller:"
  putStrLn $ showDiagnosticInfoMaybe2L seller8

-- | Hand-rolling the specific output type for publish subgame
printOutputPublishSubgame strategy parameters = do
  let  buyer5 ::- buyer6 ::- seller7 ::- seller8 ::- Nil = equilibriumPublishSubGame strategy parameters
  putStrLn "LH Publish Decision by Buyer:"
  putStrLn $ showDiagnosticInfoL buyer5
  putStrLn "LH Publish Decision by Buyer - gasPub:"
  putStrLn $ showDiagnosticInfoL buyer6
  putStrLn "Fulfill Decision by Seller:"
  putStrLn $ showDiagnosticInfoMaybeL seller7
  putStrLn "NoFulfill Decision by Seller:"
  putStrLn $ showDiagnosticInfoMaybeL seller8

-- 4. Outputting only Bool info for equilibrium breaking
-- | Hand-rolling the specific output type for complete game
breakEquilibriumCompleteGame strategy parameters =
  let buyer1 ::- buyer2 ::- seller3 ::- buyer4 ::- buyer5 ::- buyer6 ::- seller7 ::- seller8 ::- Nil = equilibriumCompleteGame strategy parameters
      ls = [toEquilibriumL buyer1, toEquilibriumMaybeL buyer2, toEquilibriumMaybeL seller3, toEquilibriumMaybe2L buyer4, toEquilibriumMaybe2L buyer5, toEquilibriumMaybe2L buyer6, toEquilibriumMaybe3L seller7, toEquilibriumMaybe3L seller8]
      in and ls

-- | Hand-rolling the specific output type for accept subgame
breakEquilibriumAcceptSubgame strategy parameters =
  let seller3 ::- buyer4 ::- buyer5 ::- buyer6 ::- seller7 ::- seller8 ::- Nil = equilibriumAcceptSubGame strategy parameters
      ls = [toEquilibriumL seller3, toEquilibriumMaybeL buyer4, toEquilibriumMaybeL buyer5, toEquilibriumMaybeL buyer6, toEquilibriumMaybe2L seller7, toEquilibriumMaybe2L seller8]
      in and ls

-- | Hand-rolling the specific output type for publish subgame
breakEquilibriumPublishSubgame strategy parameters =
  let  buyer5 ::- buyer6 ::- seller7 ::- seller8 ::- Nil = equilibriumPublishSubGame strategy parameters
       ls = [toEquilibriumL buyer5, toEquilibriumL buyer6, toEquilibriumMaybeL seller7, toEquilibriumMaybeL seller8]
       in and ls
