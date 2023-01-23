{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Parameterization
  where

import OpenGames.Engine.Engine

import Analytics
import Diagnostics
import Model
import Payoffs
import Types 

{-
Defines the concrete parameterizations used for the analysis
-}

---------------------
-- 1. Strategies used
-- | Aggregate strategy tuple for complete game
-- Define all strategies in one place
testStrategy = Strategy
   initiateStrategyBuyerTarget
   noLHPublishStrategyTarget
   acceptStrategyTarget
   recoupStrategyTarget
   lhPublishStrategyPart1Target
   lhPublishStrategyPart2Target
   fulfillStrategyTarget
   noFulfillStrategyTarget

testStrategyTupleTarget = completeStrategy testStrategy

------------------------------
-- 2. Contract Parameters used

testContract = HLContract
   10
   20
   1
   5
   2

testTransaction = Transaction
  2
  10
  50

--------------------------------------
-- 3. Uncertainty and action space gas
testDistribution = uniformDist [1.0,2.0,3.0,4.0,5.0]

testActionSpaceGasPub = [6.0,8.0,10.0,12.0,14.0]

-------------------------
-- 4. Complete parameters
parameters = Parameters
  "buyer"
  "seller"
  testDistribution
  testActionSpaceGasPub
  testTransaction
  testContract
  3
  
