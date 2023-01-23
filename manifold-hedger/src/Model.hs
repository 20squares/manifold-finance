{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Model where

import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import ActionSpaces
import Components
import Payoffs
import Types
import Control.Arrow (Arrow(first))

{-
This file contains the complete model and the relevant subgames
We begin "backwards" by considering the last states first
-}


--------------
-- 1. Subgames
-- | Initiate ~> Accepted ~> Publish subgame
publishSubgame buyerName sellerName distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx, contract ;
   feedback  : ;

   :----------------------------:
   inputs    : tx, contract ;
   feedback  : ;
   operation : publishLHBuyerRandom buyerName distribution  possibleGasPubLS;
   outputs   : publishDecisionGame ;
   returns   :  ;

   inputs    : publishDecisionGame ;
   feedback  : ;
   operation : publishBranching buyerName sellerName utilityFunctionBuyer utilityFunctionSeller ;
   outputs   : fulfillDecision ;
   returns   :  ;

   :----------------------------:

   outputs   :  ;
   returns   : ;
  |]
 where
   publishBranching buyerName sellerName utilityFunctionBuyer utilityFunctionSeller  = (fulfillLHSellerPublished buyerName sellerName utilityFunctionBuyer utilityFunctionSeller) +++ (fulfillLHSellerNoOp buyerName sellerName utilityFunctionBuyer utilityFunctionSeller)


-- | Initiate ~> Accepted subgame
acceptSubgame buyerName sellerName distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx, contract,piInitial ;
   feedback  : ;

   :----------------------------:
   inputs    : tx, contract,piInitial ;
   feedback  : ;
   operation : acceptLHSeller sellerName utilityFunctionSeller;
   outputs   : acceptanceDecisionGame ;
   returns   :  ;

   inputs    : acceptanceDecisionGame ;
   feedback  : ;
   operation : acceptBranching buyerName sellerName distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller;
   outputs   : discard;
   returns   : ;

   :----------------------------:

   outputs   : ;
   returns   : ;
  |]
 where
   acceptBranching  buyerName sellerName distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller = (recoupLHBuyerRandom buyerName sellerName distribution utilityFunctionBuyer utilityFunctionSeller) +++ (publishSubgame buyerName sellerName distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller)

 
-- | Complete game
completeGame buyerName sellerName distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx,contract,piInitial ;
   feedback  : ;

   :----------------------------:
   inputs    : tx,contract,piInitial ;
   feedback  : ;
   operation : initLHBuyer buyerName utilityFunctionBuyer;
   outputs   : contractDecisionGame ;
   returns   :  ;

   inputs    : contractDecisionGame ;
   feedback  : ;
   operation : completeBranching buyerName sellerName distribution utilityFunctionBuyer utilityFunctionSeller;
   outputs   : discard;
   returns   : ;

   :----------------------------:

   outputs   :  ;
   returns   :  ;
  |]
  where
    completeBranching buyerName sellerName distribution utilityFunctionBuyer utilityFunctionSeller = (noLHBuyerRandom buyerName sellerName distribution utilityFunctionBuyer utilityFunctionSeller) +++ (acceptSubgame buyerName sellerName distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller)


