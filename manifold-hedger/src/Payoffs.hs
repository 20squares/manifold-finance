{-# LANGUAGE RecordWildCards #-}

module Payoffs
  where

import Types

-- | Payoff for initiating the contract
initialPayoffBuyer :: Transaction -> InitialDecisionBuyer HLContract -> GasPrice -> Payoff
initialPayoffBuyer _ Wait _ = 0
initialPayoffBuyer Transaction{..} (Initiate HLContract{..}) price = - (gasInitiation * price ) - payment - epsilon -- ^ TODO check the payment and epsilon component later

-- | Payoff for buyer when no initialized contract
noLHPayoffBuyer :: Transaction -> GasPrice -> PublishDecision -> Payoff
noLHPayoffBuyer _ _ NoOp = 0
noLHPayoffBuyer Transaction{..} price Publish = utilityFromTX - (gasAllocTX * price)

-- | Payoff for seller when no initialized contract
noLHPayoffSeller :: Transaction -> GasPrice -> Payoff
noLHPayoffSeller Transaction{..} price = gasAllocTX * price

-- | Payoff for seller when accepting the hl contract
-- TODO check units for collateral
acceptLHPayoffSeller :: HLContract -> GasPrice -> Payoff
acceptLHPayoffSeller HLContract{..} price = - (gasAccept * price) - collateral
