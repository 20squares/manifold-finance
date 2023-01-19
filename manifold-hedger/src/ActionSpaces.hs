module ActionSpaces
  where

import Types

-- | Initiate decision buyer
-- Given the observation, the contract and the current gas price, the player can choose to initiate or wait
actionSpaceInitLHBuyer (_, contract,_) = [Wait, Initiate contract]

-- | Publish decision buyer
-- Given the observed price, choose the gas price 
actionSpacePublishLHBuyer
  :: [Gas]
     -> (a2, b, PublishDecision a3)
     -> [(PublishDecision Gas)]
actionSpacePublishLHBuyer possibleGas (pi,tx,publishDecision) =
  case publishDecision of
    NoOp      -> [NoOp]
    Publish _ -> fmap Publish possibleGas

-- | Transform initiate decision into game branching choice
transformInitiateDecision (Wait               , tx ,_) =  Left tx
transformInitiateDecision ((Initiate contract), tx, pi )= Right (tx,contract,pi)

-- | Transform accept decision into game choice
transformAcceptDecision Decline information = Left information
transformAcceptDecision Accept  information = Right information

-- | Transform publish decision into game choice
transformPublishDecision (Publish gasPub) (tx,contract,pi) = Left (tx,contract,pi,gasPub)
transformPublishDecision NoOp    information = Right information
