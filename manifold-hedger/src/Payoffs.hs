{-# LANGUAGE RecordWildCards #-}

module Payoffs
  where

import Types

-- | PayoffHL for buyer when no initialized contract
-- Paper:
  -- Buyer
      -- NoOp : wealthBuyer
      -- Publish gasPub -> gasAllocTX : wealthBuyer + utilityFromTX - gasAllocTX * priceNew
noLHPayoffBuyer :: Wealth -> (Transaction, GasPrice, (PublishDecision Gas)) -> PayoffHL
noLHPayoffBuyer wealthBuyer (_, _, NoOp) = wealthBuyer
noLHPayoffBuyer wealthBuyer (Transaction{..}, priceNew, (Publish _)) = wealthBuyer + utilityFromTX - (gasAllocTX * priceNew)

-- | PayoffHL for seller when no initialized contract
-- Paper:
   -- Seller
         --  Sells his blockspace : wealthSeller + gasAllocTX * priceNew
noLHPayoffSeller :: Wealth -> Transaction -> GasPrice -> PayoffHL
noLHPayoffSeller wealthSeller Transaction{..} priceNew = wealthSeller +  gasAllocTX * priceNew


------------------------------
-- Payoffs after initialization
-- In all games, the buyer must carry the initialization costs
-- | Alias for the recoup case
-- Paper:
   -- Seller
         --  Sells his blockspace : wealthSeller + gasAllocTX * priceNew
recoupLHPayoffSeller = noLHPayoffSeller

-- | PayoffHL for buyer when recouping the hl contract
-- Paper:
    -- Buyer:
        -- Recoup (Refund): wealthBuyer + (payment + epsilon) 
        --                                 ^^ transfer back from contract
        --                              - gasDone * priceNew
        --                                 ^^ publication costs for recoup
        --                              - (payment + epsilon + gasInit * priceOld )
        --                                 ^^ initiation costs
        --                              + maximum [0, utilityFromTX - (gasAllocTx * priceNew)]
        --                                 ^^ buyer can still do the transaction if it is worth it 
        -- <=>            : wealthBuyer - gasDone * priceNew - gasInit * priceOld  + maximum [0, utilityFromTX - (gasAllocTx * priceNew)]
        --
        -- Forfeit        : wealthBuyer - (payment + epsilon + gasInit * priceOld)
        --                                 ^^ initiation costs of the contract
        --                              + maximum [0, utilityFromTX - (gasAllocTx * priceNew)]
        --                                 ^^ buyer can still do the transaction if it is worth it
recoupLHPayoffBuyer :: Wealth -> (Transaction, HLContract, GasPrice, GasPrice, RecoupDecisionBuyer) ->  PayoffHL
recoupLHPayoffBuyer wealthBuyer (Transaction{..}, HLContract{..}, priceNew, priceOld, decision) =
  case decision of
    Refund  -> wealthBuyer + maximum [0,netUtility] + payment + epsilon - priceNew * gasDone + costsInitialization
    Forfeit -> wealthBuyer + maximum [0,netUtility] + costsInitialization
  where
    netUtility = utilityFromTX - (gasAllocTX * priceNew)
    costsInitialization = - (gasInitiation * priceOld ) - payment - epsilon 

------------
-- In all subgames from here on, the seller must carry the acceptance costs 
-- | PayoffHL for seller when fullfilling the contract
-- Paper:
    -- Seller:
        -- Exhaust : wealthSeller + (payment + collateral)
        --                           ^^ transfer from contract
        --                        - gasAllocTx * priceNew
        --                           ^^ costs from null operation (??)
        --                        + gasAllocTX * priceNew
        --                           ^^ the seller holds the payload and can offeset the requirements (??)
        --                        - gasDone * priceNew
        --                           ^^ costs for remaining operations (??)
        --                        - (gasAccept * priceOld) - collateral
        --                           ^^ acceptance costs
        -- <=>    : wealthSeller  + payment  - gasDone * priceNew - gasAccept * priceOld
        -- Ignore : wealthSeller  + gasAllocTX * priceNew
        --                           ^^ the seller can sell his payload
        --                        - (gasAccept * priceOld) - collateral
        --                           ^^ acceptance costs
        -- Confirm: wealthSeller  + (payment + collateral + epsilon)
        --                        + gasAllocTx * priceNew
        --                           ^^ selling gas
        --                        - gasPub * priceNew
        --                           ^^ required gas
        --                        - gasDone * priceNew
        --                           ^^ publication costs
        --                        - (gasAccept * priceOld) - collateral
        --                           ^^ acceptance costs
        -- NOTE if gasPub==gasAllocTX =>
        --          wealthSeller  + (payment + collateral + epsilon) - gasDone * priceNew  - (gasAccept * priceOld) - collateral
fulfillLHPayoffSeller :: Wealth -> (Transaction, HLContract, Gas, GasPrice,GasPrice,FulfillDecisionSeller) -> PayoffHL
fulfillLHPayoffSeller wealthSeller (Transaction{..}, HLContract{..}, gasPub, priceNew, priceOld, decision) =
  case decision of
    Exhaust -> wealthSeller + payment + collateral - (gasDone * priceNew) + costsAcceptance
    Ignore  -> wealthSeller + gasAllocTX * priceNew + costsAcceptance
    Confirm -> wealthSeller + payment + collateral + epsilon + ((-gasDone + (gasAllocTX - gasPub)) * priceNew) +  costsAcceptance
  where
    costsAcceptance = ((-gasAccept) * priceOld) - collateral

-- | PayoffHL for seller when not fullfilling the contract
-- Paper:
    -- Seller:
        -- Exhaust : wealthSeller + (payment + collateral)
        --                           ^^ transfer from contract
        --                        - gasAllocTx * priceNew
        --                           ^^ costs from null operation (??)
        --                        + gasAllocTX * priceNew
        --                           ^^ the seller holds the payload and can offeset the requirements (??)
        --                        - gasDone * priceNew
        --                           ^^ costs for remaining operations (??)
        --                        - (gasAccept * priceOld) - collateral
        --                           ^^ acceptance costs
        -- <=>    : wealthSeller  + payment  - gasDone * priceNew - gasAccept * priceOld
        -- Ignore : wealthSeller  + gasAllocTX * priceNew
        --                           ^^ the seller can sell his payload
        --                        - (gasAccept * priceOld) - collateral
        --                           ^^ acceptance costs
noFulfillLHPayoffSeller :: Wealth -> (Transaction, HLContract, GasPrice, GasPrice, FulfillDecisionSeller) -> PayoffHL
noFulfillLHPayoffSeller wealthSeller ( Transaction{..}, HLContract{..}, priceNew, priceOld, decision) =
  case decision of
    Exhaust -> wealthSeller + payment + collateral - (gasDone * priceNew) + costsAcceptance
    Ignore  -> wealthSeller + gasAllocTX * priceNew + costsAcceptance
  where
    costsAcceptance =  ((-gasAccept) * priceOld) - collateral

-- | PayoffHL for buyer conditional on the fulfillment decision
-- NOTE we build in the decision to transact when exhaust or ignore decisions by seller are made
-- Paper:
    -- Buyer:
        -- Exhaust:  wealthBuyer - (payment + epsilon + gasInit * priceOld)
        --                           ^^ initiation costs of the contract
        --                       + maximum [0, utilityFromTX - (gasAllocTx * priceNew)]
        --                           ^^ buyer can still do the transaction if it is worth it
        -- Ignore:  wealthBuyer  - (payment + epsilon + gasInit * priceOld)
        --                           ^^ initiation costs of the contract
        --                       + maximum [0, utilityFromTX - (gasAllocTx * priceNew)]
        --                           ^^ buyer can still do the transaction if it is worth it
        -- Confirm: wealthBuyer - (payment + epsilon + gasInit * priceOld)
        --                           ^^ initiation costs of the contract
        --                      + utilityFromTX 
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
-- Paper:
    -- Buyer:
        -- Exhaust:  wealthBuyer - (payment + epsilon + gasInit * priceOld)
        --                           ^^ initiation costs of the contract
        --                       + maximum [0, utilityFromTX - (gasAllocTx * priceNew)]
        --                           ^^ buyer can still do the transaction if it is worth it
        -- Ignore:  wealthBuyer  - (payment + epsilon + gasInit * priceOld)
        --                           ^^ initiation costs of the contract
        --                       + maximum [0, utilityFromTX - (gasAllocTx * priceNew)]
        --                           ^^ buyer can still do the transaction if it is worth it
noFulfillLHPayoffBuyer :: Wealth -> (Transaction, HLContract, GasPrice, GasPrice, FulfillDecisionSeller) -> PayoffHL
noFulfillLHPayoffBuyer wealthBuyer ( Transaction{..}, HLContract{..}, priceNew, priceOld, decision) =
  case decision of
    Exhaust -> wealthBuyer + maximum [0,netUtility] + costsInitialization
    Ignore  -> wealthBuyer + maximum [0,netUtility] + costsInitialization
  where
    netUtility = utilityFromTX - (gasAllocTX * priceNew)
    costsInitialization = - (gasInitiation * priceOld ) - payment - epsilon 

