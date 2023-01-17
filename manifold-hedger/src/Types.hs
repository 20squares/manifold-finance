{-# LANGUAGE DataKinds #-}

module Types where


-- 1. Action types

-- | Initial decision buyer
data InitialDecisionBuyer a = Wait | Initiate a
     deriving (Eq,Ord,Show)

-- | Publication decision buyer
data PublishDecision = Publish | NoOp
  deriving (Eq,Ord,Show)

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
  } deriving (Eq,Show,Ord)

-- | Transaction that the buyer wants to get implemented
-- TODO: we assume that gasAllocTX are known at the beginning here
data Transaction = Transaction
  { gasInitiation :: Gas
  , gasAllocTX    :: Gas
  , utilityFromTX :: Utility
  } deriving (Eq,Ord,Show)

-- 3. Payoff types
type Payoff = Double

