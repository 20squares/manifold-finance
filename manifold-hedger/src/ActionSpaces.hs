module ActionSpaces
  where

import Types

-- | Initiate decision buyer
-- Given the observation, the contract and the current gas price, the player can choose to initiate or wait
actionSpaceInitLHBuyer (_, contract,_) = [Wait, Initiate contract]

-- | Transform initiate decision into game branching choice
transformInitiateDecision Wait                =  Left ()
transformInitiateDecision (Initiate contract) = Right contract

-- | Transform accept decision into game choice
transformAcceptDecision Decline = Left ()
transformAcceptDecision Accept  = Right ()

-- | Transform publish decision into game choice
transformPublishDecision Publish information = Left information
transformPublishDecision NoOp    information = Right information
