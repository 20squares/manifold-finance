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
-- | Buyer: initiate contract strategy 
initiateStrategyBuyerTarget
  :: Kleisli
           Stochastic
           (Transaction, HLContract, GasPrice)
           (InitialDecisionBuyer HLContract)
initiateStrategyBuyerTarget =
  Kleisli (\(_,contract,_) -> playDeterministically $ Initiate contract)

-- | Buyer: publish strategy if no LH
noLHPublishStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, GasPrice)
       (PublishDecision Double)
noLHPublishStrategyTarget =  Kleisli (\(tx,_ ) -> playDeterministically $ Publish (gasAllocTX tx))

-- | Seller: accept decision
acceptStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice)
       AcceptDecisionSeller
acceptStrategyTarget = pureAction Accept


-- | Buyer: publish strategy if recoup
recoupPublishTarget
  :: Kleisli
       Stochastic
       (Transaction, GasPrice)
       (PublishDecision Double)
recoupPublishTarget =  Kleisli (\(tx,_ ) -> playDeterministically $ Publish (gasAllocTX tx))

-- | Buyer: recoup strategy buyer
recoupStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice, GasPrice)
       RecoupDecisionBuyer
recoupStrategyTarget = pureAction Refund

-- | Buyer: publish strategy part 1 if LH
lhPublishStrategyPart1Target
  :: Kleisli
       Stochastic
       GasPrice
       (PublishDecision Double)
lhPublishStrategyPart1Target =  pureAction $ Publish 0.0

-- | Buyer: publish strategy part 2 if LH
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

-- | Seller: fulfill strategy
fulfillStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice,GasPrice, Gas)
       FulfillDecisionSeller
fulfillStrategyTarget = pureAction Confirm

-- | Seller: noFulfill strategy
noFulfillStrategyTarget
  :: Kleisli
       Stochastic
       (Transaction, HLContract, GasPrice, GasPrice)
       FulfillDecisionSeller
noFulfillStrategyTarget = pureAction Exhaust

-- | Buyer: publish strategy if no fulfill
nofulfillPublishTarget
  :: Kleisli
       Stochastic
       (Transaction, GasPrice)
       (PublishDecision Double)
nofulfillPublishTarget =  Kleisli (\(tx,_ ) -> playDeterministically $ Publish (gasAllocTX tx))

