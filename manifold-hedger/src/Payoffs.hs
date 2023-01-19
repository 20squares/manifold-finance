{-# LANGUAGE RecordWildCards #-}

module Payoffs
  where

import Types

-- | Payoff for initiating the contract
initialPayoffBuyer :: Transaction -> InitialDecisionBuyer HLContract -> GasPrice -> Payoff
initialPayoffBuyer _ Wait _ = 0
initialPayoffBuyer Transaction{..} (Initiate HLContract{..}) price = - (gasInitiation * price ) - payment - epsilon -- ^ TODO check the payment and epsilon component later

-- | Payoff for buyer when no initialized contract
noLHPayoffBuyer :: Transaction -> GasPrice -> (PublishDecision Gas) -> Payoff
noLHPayoffBuyer _ _ NoOp = 0
noLHPayoffBuyer Transaction{..} price (Publish _) = utilityFromTX - (gasAllocTX * price)

-- | Payoff for seller when no initialized contract
noLHPayoffSeller :: Transaction -> GasPrice -> Payoff
noLHPayoffSeller Transaction{..} price = gasAllocTX * price

-- | Payoff for seller when accepting the hl contract
-- TODO check units for collateral
acceptLHPayoffSeller :: HLContract -> GasPrice -> Payoff
acceptLHPayoffSeller HLContract{..} price = - (gasAccept * price) - collateral

-- | Payoff for buyer when recouping the hl contract
recoupLHPayoffBuyer :: HLContract -> GasPrice ->  RecoupDecisionBuyer ->  Payoff
recoupLHPayoffBuyer HLContract{..} price Refund  = payment + epsilon - price * gasDone -- ^ TODO Check the payment and epsilon
recoupLHPayoffBuyer HLContract{..} price Forfeit = 0 

-- | Payoff for seller when fullfilling the contract
fulfillLHPayoffSeller :: Transaction -> HLContract -> Gas ->  GasPrice -> FulfillDecisionSeller -> Payoff
fulfillLHPayoffSeller Transaction{..} HLContract{..} _      price Exhaust = payment + collateral - (gasDone * price)
fulfillLHPayoffSeller Transaction{..} HLContract{..} _      price Ignore  = gasAllocTX * price
fulfillLHPayoffSeller Transaction{..} HLContract{..} gasPub price Confirm = payment + collateral + epsilon - ((gasAllocTX - (gasPub + gasDone))*price)

-- | Payoff for seller when not fullfilling the contract
noFulfillLHPayoffSeller :: Transaction -> HLContract  ->  GasPrice -> FulfillDecisionSeller -> Payoff
noFulfillLHPayoffSeller Transaction{..} HLContract{..}   price Exhaust = payment + collateral - (gasDone * price)
noFulfillLHPayoffSeller Transaction{..} HLContract{..}   price Ignore  = gasAllocTX * price

-- | Payoff for buyer conditional on the fulfillment decision
-- NOTE we build in the decision to transact when exhaust or ignore decisions by seller are made
fulfillLHPayoffBuyer :: (Transaction, HLContract, GasPrice, FulfillDecisionSeller) -> Payoff
fulfillLHPayoffBuyer ( Transaction{..}, HLContract{..}, price, decision) =
  case decision of
    Exhaust -> maximum [0,netUtility]
    Ignore  -> maximum [0,netUtility]
    Confirm -> utilityFromTX -- ^ FIXME ?
  where netUtility = utilityFromTX - (gasAllocTX * price)

