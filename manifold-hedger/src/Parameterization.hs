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
import Strategies
import Types

import Numeric.Probability.Distribution (normal)

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
   (10**9)
   100
   1
   (0.1*10**9)
   (75*10**3)
   (20*10**3)

testTransaction = Transaction
  (5 * 10**6)
  (10**9)

--------------------------------------
-- 3. Uncertainty and action space gas
testDistribution = normal [0..200]

testActionSpaceGasPub = [0,(5 * 10**6)]


--------------------------
-- 4. Utility functions
logUtility x = log x

squareRootUtility x = sqrt x
-------------------------
-- 5. Complete parameters
parameters = Parameters
  "buyer"
  "seller"
  testDistribution
  testActionSpaceGasPub
  testTransaction
  testContract
  100
  id
  id
