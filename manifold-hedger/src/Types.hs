module Types where

data InitialDecisionBuyer = Wait | Initiate
  deriving (Eq,Ord,Show)

data PublishDecision = Publish | NoOp
  deriving (Eq,Ord,Show)

data RecoupDecisionBuyer = Refund | Forfeit
  deriving (Eq,Ord,Show)

data AcceptDecisionSeller = Decline | Accept
  deriving (Eq,Ord,Show)

data FulfillDecisionSeller = Confirm | Exhaust | Ignore
  deriving (Eq,Ord,Show)




