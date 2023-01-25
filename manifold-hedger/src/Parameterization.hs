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
import Numeric.Probability.Distribution (shape)
import Numeric.Probability.Shape (normalCurve)

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
   (0.1*10**6)
   (75*10**3)
   (20*10**3)

testTransaction = Transaction
  (5 * 10**6)
  (10**9)

--------------------------------------
-- 3. Uncertainty and action space gas
normalDistribution standardDeviationParameter = shape normal [0..200]
  where
   normal = normalCurve 100 (10**(standardDeviationParameter+1))

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
  (10**9)
  (10**9)
  (normalDistribution 3)
  testActionSpaceGasPub
  testTransaction
  testContract
  100
  logUtility
  logUtility
