{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  where


import qualified Data.ByteString.Lazy as L
import           Data.Csv
import           Data.Vector
import           GHC.Generics

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

newtype GasPriceNew = GasPriceNew Double

-- | HL contract
data HLContract = HLContract
  { collateral    :: Collateral
  , payment       :: Payment
  , epsilon       :: Payment
  , gasInitiation :: Gas
  , gasAccept     :: Gas
  , gasDone       :: Gas
  } deriving (Eq,Show,Ord)

-- | Transaction that the buyer wants to get implemented
-- TODO: we assume that gasAllocTX are known at the beginning here
data Transaction = Transaction
  { gasAllocTX    :: Gas
  , utilityFromTX :: Utility
  } deriving (Eq,Ord,Show)

-- 3. Payoff types
type PayoffHL = Double
type UtilityFunction = (PayoffHL -> OpenGames.Engine.Engine.Payoff)
type Wealth = Double

-- 4. Interface type
data Parameters = Parameters
  { buyerName :: String
  , sellerName :: String
  , buyerWealth :: Wealth
  , sellerWealth :: Wealth
  , distribution :: Stochastic Gas
  , actionSpaceGasPub :: [Gas]
  , transaction :: Transaction
  , contract :: HLContract
  , piInitial :: GasPrice
  , utilityFunctionBuyer :: UtilityFunction
  , utilityFunctionSeller :: UtilityFunction
  }

-- 5. Import distribution

type ProbabilityMass = Double

data ImportProbabilityTuple = ImportProbabilityTuple
  { value :: GasPrice
  , probMass :: ProbabilityMass
  } deriving (Generic,Show)

instance FromNamedRecord ImportProbabilityTuple
instance DefaultOrdered ImportProbabilityTuple

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
                                (Transaction, HLContract, GasPrice, GasPrice)
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
                                (Transaction, HLContract, GasPrice, GasPrice, Gas)
                                FulfillDecisionSeller
  , noFulfillStrategy      :: Kleisli
                                Stochastic
                                (Transaction, HLContract, GasPrice, GasPrice)
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


