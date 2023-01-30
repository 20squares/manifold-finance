{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Strategies
  where

import OpenGames.Engine.Engine

import Analytics
import Diagnostics
import Model
import Payoffs
import Types

{-
Defines the strategies
-}

----------------
-- 1. Strategies
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
       (Transaction, HLContract, GasPrice, GasPrice)
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
       (Transaction, HLContract, GasPrice,GasPrice, Gas)
       FulfillDecisionSeller
fulfillStrategyTarget = pureAction Confirm

-- | noFulfill strategy
noFulfillStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice, GasPrice)
       FulfillDecisionSeller
noFulfillStrategyTarget = pureAction Exhaust

