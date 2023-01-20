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
-- | Equilibrium definition
equilibriumCompleteGame strategy Parameters{..} = evaluate (completeGame buyerName sellerName distribution actionSpaceGasPub) strategy context
  where
    context =
      StochasticStatefulContext
         (pure ((),(transaction,contract,piInitial))) (\_ _ -> pure ())

-- | Hand-rolling the specific output type
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


----------------
-- 2. Strategies
-- | initiate contract strategy
initiateStrategyBuyer
  :: Kleisli
           Stochastic
           (Transaction, HLContract, GasPrice)
           (InitialDecisionBuyer HLContract)
initiateStrategyBuyer =
  Kleisli (\(_,contract,_) -> playDeterministically $ Initiate contract)

-- | publish strategy if no LH
noLHPublishStrategy
  :: Kleisli
       Stochastic
       (Transaction, GasPrice)
       (PublishDecision Double)
noLHPublishStrategy = pureAction $ Publish 0.0

-- | accept decision seller
acceptStrategy
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice)
       AcceptDecisionSeller
acceptStrategy = pureAction Accept

-- | recoup strategy buyer
recoupStrategy
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice)
       RecoupDecisionBuyer
recoupStrategy = pureAction Refund

-- | publish strategy part 1 if LH
lhPublishStrategyPart1
  :: Kleisli
       Stochastic
       GasPrice
       (PublishDecision Double)
lhPublishStrategyPart1 =  pureAction $ Publish 0.0

-- | publish strategy part 2 if LH
lhPublishStrategyPart2
  ::  Kleisli
          Stochastic
          (GasPrice, Transaction, PublishDecision a1)
          (PublishDecision Gas)
lhPublishStrategyPart2 =
  Kleisli
   (\(pi,tx,publishDecision) -> 
        case publishDecision of
          NoOp -> playDeterministically NoOp
          Publish _ -> playDeterministically $ Publish $ gasAllocTX tx)

-- | fulfill strategy
fulfillStrategy
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice, Gas)
       FulfillDecisionSeller
fulfillStrategy = pureAction Confirm

-- | noFulfill strategy
noFulfillStrategy
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice)
       FulfillDecisionSeller
noFulfillStrategy = pureAction Exhaust

-- Aggregate strategy tuple
strategyTuple =
  initiateStrategyBuyer
  ::- noLHPublishStrategy
  ::- acceptStrategy
  ::- recoupStrategy
  ::- lhPublishStrategyPart1
  ::- lhPublishStrategyPart2
  ::- fulfillStrategy
  ::- noFulfillStrategy
  ::- Nil

---------------------
-- 3. Parameters used

testContract = HLContract
   10
   20
   1
   5
   2

testTransaction = Transaction
  2
  10
  50

testDistribution = uniformDist [1.0,2.0,3.0,4.0,5.0]

testActionSpaceGasPub = [6.0,8.0,10.0,12.0,14.0]

parameters = Parameters
  "buyer"
  "seller"
  testDistribution
  testActionSpaceGasPub
  testTransaction
  testContract
  3
  
