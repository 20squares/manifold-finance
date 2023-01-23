{-# LANGUAGE RecordWildCards #-}

module Payoffs
  where

import Types

-- | PayoffHL for initiating the contract
initialPayoffBuyer :: Transaction -> InitialDecisionBuyer HLContract -> GasPrice -> PayoffHL
initialPayoffBuyer _ Wait _ = 0
initialPayoffBuyer Transaction{..} (Initiate HLContract{..}) price = - (gasInitiation * price ) - payment - epsilon 

-- | PayoffHL for buyer when no initialized contract
noLHPayoffBuyer :: Transaction -> GasPrice -> (PublishDecision Gas) -> PayoffHL
noLHPayoffBuyer _ _ NoOp = 0
noLHPayoffBuyer Transaction{..} price (Publish _) = utilityFromTX - (gasAllocTX * price)

-- | PayoffHL for seller when no initialized contract
noLHPayoffSeller :: Transaction -> GasPrice -> PayoffHL
noLHPayoffSeller Transaction{..} price = gasAllocTX * price

-- | Alias for the recoup case
recoupLHPayoffSeller = noLHPayoffSeller

-- | PayoffHL for seller when accepting the hl contract
acceptLHPayoffSeller :: HLContract -> GasPrice -> PayoffHL
acceptLHPayoffSeller HLContract{..} price = - (gasAccept * price) - collateral

-- | PayoffHL for buyer when recouping the hl contract
recoupLHPayoffBuyer :: Transaction -> HLContract -> GasPrice ->  RecoupDecisionBuyer ->  PayoffHL
recoupLHPayoffBuyer Transaction{..} HLContract{..} price Refund  = payment + epsilon - price * gasDone 
recoupLHPayoffBuyer Transaction{..} HLContract{..} price Forfeit = maximum [0,netUtility]
  where
    netUtility = utilityFromTX - (gasAllocTX * price)


-- | PayoffHL for seller when fullfilling the contract
fulfillLHPayoffSeller :: Transaction -> HLContract -> Gas ->  GasPrice -> FulfillDecisionSeller -> PayoffHL
fulfillLHPayoffSeller Transaction{..} HLContract{..} _      price Exhaust = payment + collateral - (gasDone * price)
fulfillLHPayoffSeller Transaction{..} HLContract{..} _      price Ignore  = gasAllocTX * price
fulfillLHPayoffSeller Transaction{..} HLContract{..} gasPub price Confirm = payment + collateral + epsilon - ((gasAllocTX - (gasPub + gasDone))*price)

-- | PayoffHL for seller when not fullfilling the contract
noFulfillLHPayoffSeller :: Transaction -> HLContract  ->  GasPrice -> FulfillDecisionSeller -> PayoffHL
noFulfillLHPayoffSeller Transaction{..} HLContract{..}   price Exhaust = payment + collateral - (gasDone * price)
noFulfillLHPayoffSeller Transaction{..} HLContract{..}   price Ignore  = gasAllocTX * price

-- | PayoffHL for buyer conditional on the fulfillment decision
-- NOTE we build in the decision to transact when exhaust or ignore decisions by seller are made
fulfillLHPayoffBuyer :: (Transaction, HLContract, GasPrice, FulfillDecisionSeller) -> PayoffHL
fulfillLHPayoffBuyer ( Transaction{..}, HLContract{..}, price, decision) =
  case decision of
    Exhaust -> maximum [0,netUtility]
    Ignore  -> maximum [0,netUtility]
    Confirm -> utilityFromTX -- ^ FIXME ?
  where
    netUtility = utilityFromTX - (gasAllocTX * price)

