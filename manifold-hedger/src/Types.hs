{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}

module Types
  where

import OpenGames.Engine.Engine

-- 1. Action types

-- | Initial decision buyer
data InitialDecisionBuyer a = Wait | Initiate a
     deriving (Eq,Ord,Show)

-- | Publication decision buyer
data PublishDecision a = NoOp | Publish a
  deriving (Eq,Ord,Show, Functor)

-- | Recoup decision buyer
data RecoupDecisionBuyer = Refund | Forfeit
  deriving (Eq,Ord,Show)

-- | Accept decision seller
data AcceptDecisionSeller = Decline | Accept
  deriving (Eq,Ord,Show)

-- | fulfilldecision seller
data FulfillDecisionSeller = Confirm | Exhaust | Ignore
  deriving (Eq,Ord,Show)


-- 2. Contract type
-- | Aliases
type Collateral = Double
type Payment    = Double
type Gas        = Double
type GasPrice   = Double
type Utility    = Double

-- | HL contract
data HLContract = HLContract
  { collateral    :: Collateral
  , payment       :: Payment
  , epsilon       :: Payment
  , gasAccept     :: Gas
  , gasDone       :: Gas
  } deriving (Eq,Show,Ord)

-- | Transaction that the buyer wants to get implemented
-- TODO: we assume that gasAllocTX are known at the beginning here
data Transaction = Transaction
  { gasInitiation :: Gas
  , gasAllocTX    :: Gas
  , utilityFromTX :: Utility
  } deriving (Eq,Ord,Show)

-- 3. Payoff types
type PayoffHL = Double
type UtilityFunction = (PayoffHL -> OpenGames.Engine.Engine.Payoff)

-- 4. Interface type
data Parameters = Parameters
  { buyerName :: String
  , sellerName :: String
  , distribution :: Stochastic Gas
  , actionSpaceGasPub :: [Gas]
  , transaction :: Transaction
  , contract :: HLContract
  , piInitial :: GasPrice
  , utilityFunctionBuyer :: UtilityFunction
  , utilityFunctionSeller :: UtilityFunction
  }

-- 5. strategies
-- | Define general strategy type for constructing the relevant subgame strategies
-- Should avoid accidental misspecifications
data Strategy a = Strategy
  { initiateStrategyBuyer  :: Kleisli
                                Stochastic
                                (Transaction, HLContract, GasPrice)
                                (InitialDecisionBuyer HLContract)

  , noLHPublishStrategy    :: Kleisli
                                Stochastic
                                (Transaction, GasPrice)
                                (PublishDecision Double)
  , acceptStrategy         :: Kleisli
                                Stochastic
                                (Transaction, HLContract, GasPrice)
                                AcceptDecisionSeller
  , recoupStrategy         :: Kleisli
                                Stochastic
                                (Transaction, HLContract, GasPrice)
                                RecoupDecisionBuyer
  , lhPublishStrategyPart1 :: Kleisli
                                Stochastic
                                GasPrice
                                (PublishDecision Double)
  , lhPublishStrategyPart2 :: Kleisli
                                Stochastic
                                (GasPrice, Transaction, PublishDecision a)
                                (PublishDecision Gas)
  , fulfillStrategy        :: Kleisli
                                Stochastic
                                (Transaction, HLContract, GasPrice, Gas)
                                FulfillDecisionSeller
  , noFulfillStrategy      :: Kleisli
                                Stochastic
                                (Transaction, HLContract, GasPrice)
                                FulfillDecisionSeller
  } 

-- Complete strategy description as tuple for each relevant subgame
completeStrategy Strategy{..} =
  ( initiateStrategyBuyer
    ::- noLHPublishStrategy
    ::- acceptStrategy
    ::- recoupStrategy
    ::- lhPublishStrategyPart1
    ::- lhPublishStrategyPart2
    ::- fulfillStrategy
    ::- noFulfillStrategy
    ::- Nil
  , acceptStrategy
    ::- recoupStrategy
    ::- lhPublishStrategyPart1
    ::- lhPublishStrategyPart2
    ::- fulfillStrategy
    ::- noFulfillStrategy
    ::- Nil
  , lhPublishStrategyPart1
    ::- lhPublishStrategyPart2
    ::- fulfillStrategy
    ::- noFulfillStrategy
    ::- Nil
  )


