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

import qualified Data.ByteString.Lazy as L
import           Data.Csv
import qualified Data.Vector          as V
import Numeric.Probability.Distribution (shape, norm, fromFreqs)
import Numeric.Probability.Shape (normalCurve)
import Types (Transaction(gasAllocTX))

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
   recoupPublishTarget
   recoupStrategyTarget
   lhPublishStrategyPart1Target
   lhPublishStrategyPart2Target
   fulfillStrategyTarget
   noFulfillStrategyTarget
   nofulfillPublishTarget

testStrategyTupleTarget = completeStrategy testStrategy

------------------------------
-- 2. Contract Parameters used

testContract piContract gInit gAccept gDone = HLContract
   (10**9)
   (gasAllocTX testTransaction * piContract)
   1
   gInit   -- NOTE: paper (0.1*10**6)
   gAccept -- NOTE: paper (75*10**3)
   gDone   -- NOTE: paper (20*10**3)



testTransaction = Transaction
  (5 * 10**6) -- gasAllocTX
  (10**9)     -- utilityFromTX

--------------------------------------
-- 3. Uncertainty and action space gas
-- Import from external

decodeImportProb :: L.ByteString -> Either String (Header, V.Vector ImportProbabilityTuple)
decodeImportProb content = decodeByName content

fromVectorToProbDist :: V.Vector ImportProbabilityTuple -> Stochastic GasPrice
fromVectorToProbDist = norm . fromFreqs . (fmap (\x -> (value x, probMass x))) . V.toList

testActionSpaceGasPub = [0,(5 * 10**6)]


--------------------------
-- 4. Utility functions
logUtility x = logBase 10 x

squareRootUtility x = sqrt x

exponentialUtility par x = x**(1/par)

-------------------------
-- 5. Complete parameters
parameters distribution piContract gInit gAccept gDone exponentialBuyer exponentialSeller  = Parameters
  "buyer"
  "seller"
  (10**9)                                     -- buyerWealth
  (10**9)                                     -- sellerWealth
  distribution
  testActionSpaceGasPub
  testTransaction
  (testContract piContract gInit gAccept gDone)
  100                                         -- piInitial
  (exponentialUtility exponentialBuyer)       -- utilityFunctionBuyer
  (exponentialUtility exponentialSeller)      -- utilityFunctionSeller
