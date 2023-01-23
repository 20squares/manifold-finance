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
Defines the main analytics for the model
-}

------------------------
-- 1. Equilibrium notion
-- | Equilibrium definition for complete game
equilibriumCompleteGame strategy Parameters{..} = evaluate (completeGame buyerName sellerName distribution actionSpaceGasPub) strategy context
  where
    context =
      StochasticStatefulContext
         (pure ((),(transaction,contract,piInitial))) (\_ _ -> pure ())

-- | Hand-rolling the specific output type for complete game
printOutputCompleteGame strategy parameters = do
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

-- | Equilibrium definition for accept subgame
equilibriumAcceptSubGame strategy Parameters{..} = evaluate (acceptSubgame buyerName sellerName distribution actionSpaceGasPub) strategy context
  where
    context =
      StochasticStatefulContext
         (pure ((),(transaction,contract,piInitial))) (\_ _ -> pure ())

-- | Hand-rolling the specific output type for accept subgame
printOutputAcceptSubgame strategy parameters = do
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


-- | Equilibrium definition for publish subgame
equilibriumPublishSubGame strategy Parameters{..} = evaluate (publishSubgame buyerName sellerName distribution actionSpaceGasPub) strategy context
  where
    context =
      StochasticStatefulContext
         (pure ((),(transaction,contract))) (\_ _ -> pure ())


-- | Hand-rolling the specific output type for publish subgame
printOutputPublishSubgame strategy parameters = do
  let  buyer5 ::- buyer6 ::- seller7 ::- seller8 ::- Nil = equilibriumPublishSubGame strategy parameters
  putStrLn "LH Publish Decision by Buyer:"
  putStrLn $ checkEqL buyer5
  putStrLn "LH Publish Decision by Buyer - gasPub:"
  putStrLn $ checkEqL buyer6
  putStrLn "Fulfill Decision by Seller:"
  putStrLn $ checkEqMaybeL seller7
  putStrLn "NoFulfill Decision by Seller:"
  putStrLn $ checkEqMaybeL seller8


----------------
-- 2. Strategies
-- 2.1 Focus on target strategies (i.e. desired equilibirum behavior)
-- | initiate contract strategy 
initiateStrategyBuyerTarget
  :: Kleisli
           Stochastic
           (Transaction, HLContract, GasPrice)
           (InitialDecisionBuyer HLContract)
initiateStrategyBuyerTarget =
  Kleisli (\(_,contract,_) -> playDeterministically $ Initiate contract)

-- | publish strategy if no LH
noLHPublishStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, GasPrice)
       (PublishDecision Double)
noLHPublishStrategyTarget = pureAction $ Publish 0.0

-- | accept decision seller
acceptStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice)
       AcceptDecisionSeller
acceptStrategyTarget = pureAction Accept

-- | recoup strategy buyer
recoupStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice)
       RecoupDecisionBuyer
recoupStrategyTarget = pureAction Refund

-- | publish strategy part 1 if LH
lhPublishStrategyPart1Target
  :: Kleisli
       Stochastic
       GasPrice
       (PublishDecision Double)
lhPublishStrategyPart1Target =  pureAction $ Publish 0.0

-- | publish strategy part 2 if LH
lhPublishStrategyPart2Target
  ::  Kleisli
          Stochastic
          (GasPrice, Transaction, PublishDecision a1)
          (PublishDecision Gas)
lhPublishStrategyPart2Target =
  Kleisli
   (\(pi,tx,publishDecision) -> 
        case publishDecision of
          NoOp -> playDeterministically NoOp
          Publish _ -> playDeterministically $ Publish $ gasAllocTX tx)

-- | fulfill strategy
fulfillStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice, Gas)
       FulfillDecisionSeller
fulfillStrategyTarget = pureAction Confirm

-- | noFulfill strategy
noFulfillStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice)
       FulfillDecisionSeller
noFulfillStrategyTarget = pureAction Exhaust

