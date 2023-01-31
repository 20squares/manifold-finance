module ActionSpaces
  where

import Types
import Types (Transaction(gasAllocTX))

-- | Initiate decision buyer
-- Given the observation, the contract and the current gas price, the player can choose to initiate or wait
actionSpaceInitLHBuyer (_, contract,_) = [Wait, Initiate contract]

-- | No LH case
actionSpaceNoLHBuyer
  :: [Gas] -> (Transaction, GasPrice) -> [(PublishDecision Gas)]
actionSpaceNoLHBuyer possibleGas (tx,_ ) = fmap Publish possibleGas

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
transformInitiateDecision
  :: (InitialDecisionBuyer HLContract, Transaction, GasPrice) -> Either Transaction (Transaction, HLContract, GasPrice)
transformInitiateDecision (Wait               , tx ,_) =  Left tx
transformInitiateDecision ((Initiate contract), tx, piOld )= Right (tx,contract,piOld)

-- | Transform accept decision into game choice
transformAcceptDecision
  :: AcceptDecisionSeller -> (Transaction, HLContract, GasPrice) -> Either (Transaction, HLContract, GasPrice) (Transaction, HLContract, GasPrice)
transformAcceptDecision Decline (tx, contract, piOld) = Left (tx, contract, piOld)
transformAcceptDecision Accept  (tx, contract, piOld) = Right (tx, contract, piOld)

-- | Transform publish decision into game choice
transformPublishDecision (Publish gasPub) (tx,contract,piNew,piOld) = Left (tx,contract,piNew,piOld,gasPub)
transformPublishDecision NoOp    information = Right information
