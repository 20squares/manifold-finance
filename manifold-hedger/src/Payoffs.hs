{-# LANGUAGE RecordWildCards #-}

module Payoffs
  where

import Types

-- | PayoffHL for buyer when no initialized contract
noLHPayoffBuyer :: Wealth -> (Transaction, GasPrice, (PublishDecision Gas)) -> PayoffHL
noLHPayoffBuyer wealthBuyer (_, _, NoOp) = wealthBuyer
noLHPayoffBuyer wealthBuyer (Transaction{..}, priceNew, (Publish _)) = wealthBuyer + utilityFromTX - (gasAllocTX * priceNew)

-- | PayoffHL for seller when no initialized contract
noLHPayoffSeller :: Wealth -> Transaction -> GasPrice -> PayoffHL
noLHPayoffSeller wealthSeller Transaction{..} priceNew = wealthSeller +  gasAllocTX * priceNew


------------------------------
-- Payoffs after initialization
-- In all games, the buyer must carry the initialization costs
-- | Alias for the recoup case
recoupLHPayoffSeller = noLHPayoffSeller

-- | PayoffHL for buyer when recouping the hl contract
recoupLHPayoffBuyer :: Wealth -> (Transaction, HLContract, GasPrice, GasPrice, RecoupDecisionBuyer) ->  PayoffHL
recoupLHPayoffBuyer wealthBuyer (Transaction{..}, HLContract{..}, priceNew, priceOld, decision) =
  case decision of
    Refund  -> wealthBuyer + payment + epsilon - priceNew * gasDone + costsInitialization
    Forfeit -> wealthBuyer + maximum [0,netUtility] + costsInitialization
  where
    netUtility = utilityFromTX - (gasAllocTX * priceNew)
    costsInitialization = - (gasInitiation * priceOld ) - payment - epsilon 

------------
-- In all subgames from here on, the seller must carry the acceptance costs 
-- | PayoffHL for seller when fullfilling the contract
-- TODO Check _gasAlloc_
fulfillLHPayoffSeller :: Wealth -> (Transaction, HLContract, Gas, GasPrice,GasPrice,FulfillDecisionSeller) -> PayoffHL
fulfillLHPayoffSeller wealthSeller (Transaction{..}, HLContract{..}, gasPub, priceNew, priceOld, decision) =
  case decision of
    Exhaust -> wealthSeller + payment + collateral - (gasDone * priceNew) + costsAcceptance
    Ignore  -> wealthSeller + gasAllocTX * priceNew + costsAcceptance
    Confirm -> wealthSeller + payment + collateral + epsilon + gasAllocTX*priceOld + ((-gasDone + (gasAllocTX - gasPub)) * priceNew) +  costsAcceptance
  where
    costsAcceptance = ((-gasAccept) * priceOld) - collateral

-- | PayoffHL for seller when not fullfilling the contract
-- TODO Check _gasAlloc_
noFulfillLHPayoffSeller :: Wealth -> (Transaction, HLContract, GasPrice, GasPrice, FulfillDecisionSeller) -> PayoffHL
noFulfillLHPayoffSeller wealthSeller ( Transaction{..}, HLContract{..}, priceNew, priceOld, decision) =
  case decision of
    Exhaust -> wealthSeller + payment + collateral - (gasDone * priceNew) + costsAcceptance
    Ignore  -> wealthSeller + gasAllocTX * priceNew + costsAcceptance
  where
    costsAcceptance =  ((-gasAccept) * priceOld) - collateral

-- | PayoffHL for buyer conditional on the fulfillment decision
-- NOTE we build in the decision to transact when exhaust or ignore decisions by seller are made
fulfillLHPayoffBuyer :: Wealth -> (Transaction, HLContract, GasPrice,GasPrice,FulfillDecisionSeller) -> PayoffHL
fulfillLHPayoffBuyer wealthBuyer (Transaction{..}, HLContract{..}, priceNew, priceOld, decision) =
  case decision of
    Exhaust -> wealthBuyer + maximum [0,netUtility] + costsInitialization
    Ignore  -> wealthBuyer + maximum [0,netUtility] + costsInitialization
    Confirm -> wealthBuyer + utilityFromTX + costsInitialization
  where
    netUtility = utilityFromTX - (gasAllocTX * priceNew)
    costsInitialization = - (gasInitiation * priceOld ) - payment - epsilon 

-- | PayoffHL for buyer conditional on the fulfillment decision
-- NOTE we build in the decision to transact when exhaust or ignore decisions by seller are made
noFulfillLHPayoffBuyer :: Wealth -> (Transaction, HLContract, GasPrice, GasPrice, FulfillDecisionSeller) -> PayoffHL
noFulfillLHPayoffBuyer wealthBuyer ( Transaction{..}, HLContract{..}, priceNew, priceOld, decision) =
  case decision of
    Exhaust -> wealthBuyer + maximum [0,netUtility] + costsInitialization
    Ignore  -> wealthBuyer + maximum [0,netUtility] + costsInitialization
  where
    netUtility = utilityFromTX - (gasAllocTX * priceNew)
    costsInitialization = - (gasInitiation * priceOld ) - payment - epsilon 

