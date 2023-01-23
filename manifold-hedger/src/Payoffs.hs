{-# LANGUAGE RecordWildCards #-}

module Payoffs
  where

import Types

-- | PayoffHL for initiating the contract
initialPayoffBuyer :: Wealth -> Transaction -> InitialDecisionBuyer HLContract -> GasPrice -> PayoffHL
initialPayoffBuyer wealthBuyer _ Wait _ = wealthBuyer
initialPayoffBuyer wealthBuyer Transaction{..} (Initiate HLContract{..}) price = wealthBuyer + (- (gasInitiation * price )) - payment - epsilon 

-- | PayoffHL for buyer when no initialized contract
noLHPayoffBuyer :: Wealth -> Transaction -> GasPrice -> (PublishDecision Gas) -> PayoffHL
noLHPayoffBuyer wealthBuyer _ _ NoOp = wealthBuyer
noLHPayoffBuyer wealthBuyer Transaction{..} price (Publish _) = wealthBuyer + utilityFromTX - (gasAllocTX * price)

-- | PayoffHL for seller when no initialized contract
noLHPayoffSeller :: Wealth -> Transaction -> GasPrice -> PayoffHL
noLHPayoffSeller wealthSeller Transaction{..} price = wealthSeller +  gasAllocTX * price

-- | Alias for the recoup case
recoupLHPayoffSeller = noLHPayoffSeller

-- | PayoffHL for seller when accepting the hl contract
acceptLHPayoffSeller :: Wealth -> HLContract -> GasPrice -> PayoffHL
acceptLHPayoffSeller wealthSeller HLContract{..} price = wealthSeller + (-1.0) * ((gasAccept * price) + collateral)

-- | PayoffHL for buyer when recouping the hl contract
recoupLHPayoffBuyer :: Wealth -> Transaction -> HLContract -> GasPrice ->  RecoupDecisionBuyer ->  PayoffHL
recoupLHPayoffBuyer wealthBuyer Transaction{..} HLContract{..} price Refund  = wealthBuyer + payment + epsilon - price * gasDone 
recoupLHPayoffBuyer wealthBuyer Transaction{..} HLContract{..} price Forfeit = wealthBuyer + maximum [0,netUtility]
  where
    netUtility = utilityFromTX - (gasAllocTX * price)


-- | PayoffHL for seller when fullfilling the contract
fulfillLHPayoffSeller :: Wealth -> Transaction -> HLContract -> Gas ->  GasPrice -> FulfillDecisionSeller -> PayoffHL
fulfillLHPayoffSeller wealthSeller Transaction{..} HLContract{..} _      price Exhaust = wealthSeller + payment + collateral - (gasDone * price)
fulfillLHPayoffSeller wealthSeller Transaction{..} HLContract{..} _      price Ignore  = wealthSeller + gasAllocTX * price
fulfillLHPayoffSeller wealthSeller Transaction{..} HLContract{..} gasPub price Confirm = wealthSeller + payment + collateral + epsilon - ((gasAllocTX - (gasPub + gasDone))*price)

-- | PayoffHL for seller when not fullfilling the contract
noFulfillLHPayoffSeller :: Wealth -> Transaction -> HLContract  ->  GasPrice -> FulfillDecisionSeller -> PayoffHL
noFulfillLHPayoffSeller wealthSeller Transaction{..} HLContract{..}   price Exhaust = wealthSeller + payment + collateral - (gasDone * price)
noFulfillLHPayoffSeller wealthSeller Transaction{..} HLContract{..}   price Ignore  = wealthSeller + gasAllocTX * price

-- | PayoffHL for buyer conditional on the fulfillment decision
-- NOTE we build in the decision to transact when exhaust or ignore decisions by seller are made
fulfillLHPayoffBuyer :: Wealth -> (Transaction, HLContract, GasPrice, FulfillDecisionSeller) -> PayoffHL
fulfillLHPayoffBuyer wealthBuyer ( Transaction{..}, HLContract{..}, price, decision) =
  case decision of
    Exhaust -> wealthBuyer + maximum [0,netUtility]
    Ignore  -> wealthBuyer + maximum [0,netUtility]
    Confirm -> wealthBuyer + utilityFromTX 
  where
    netUtility = utilityFromTX - (gasAllocTX * price)

