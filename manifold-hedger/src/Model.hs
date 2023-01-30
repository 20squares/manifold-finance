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
publishSubgame  wealthBuyer wealthSeller distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx, contract,piOld ;
   feedback  : utilityBuyer2,utilitySeller2;

   :----------------------------:
   inputs    : tx, contract,piOld ;
   feedback  : utilityBuyer2,utilitySeller2;
   operation : publishLHBuyerRandom distribution  possibleGasPubLS;
   outputs   : publishDecisionGame ;
   returns   : utilityBuyer,utilitySeller;

   inputs    : publishDecisionGame ;
   feedback  : utilityBuyer,utilitySeller;
   operation : publishBranching wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller ;
   outputs   : fulfillDecision ;
   returns   :  ;

   :----------------------------:

   outputs   :  ;
   returns   : ;
  |]
 where
   publishBranching wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller  = (fulfillLHSellerPublished wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller) +++ (fulfillLHSellerNoOp wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller)

-- | Initiate ~> Accepted subgame
acceptSubgame   wealthBuyer wealthSeller distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx, contract,piOld ;
   feedback  : utilityBuyer;

   :----------------------------:
   inputs    : tx, contract,piOld ;
   feedback  : ;
   operation : acceptLHSeller  ;
   outputs   : acceptanceDecisionGame ;
   returns   : utilitySeller ;

   inputs    : acceptanceDecisionGame ;
   feedback  : utilityBuyer,utilitySeller;
   operation : acceptBranching wealthBuyer wealthSeller distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller;
   outputs   : discard;
   returns   : ;

   :----------------------------:

   outputs   : ;
   returns   : ;
  |]
 where
   acceptBranching  wealthBuyer wealthSeller distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller = (recoupLHBuyerRandom   wealthBuyer wealthSeller distribution utilityFunctionBuyer utilityFunctionSeller) +++ (publishSubgame   wealthBuyer wealthSeller distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller)

-- | Complete game
completeGame   wealthBuyer wealthSeller distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx,contract,piOld ;
   feedback  : ;

   :----------------------------:
   inputs    : tx,contract,piOld ;
   feedback  : ;
   operation : initLHBuyer  ;
   outputs   : contractDecisionGame ;
   returns   : utilityBuyer ;

   inputs    : contractDecisionGame ;
   feedback  : utilityBuyer;
   operation : completeBranching   wealthBuyer wealthSeller distribution utilityFunctionBuyer utilityFunctionSeller;
   outputs   : discard;
   returns   : ;

   :----------------------------:

   outputs   :  ;
   returns   :  ;
  |]
  where
    completeBranching   wealthBuyer wealthSeller distribution utilityFunctionBuyer utilityFunctionSeller = (noLHBuyerRandom   wealthBuyer wealthSeller distribution utilityFunctionBuyer utilityFunctionSeller) +++ (acceptSubgame   wealthBuyer wealthSeller distribution possibleGasPubLS utilityFunctionBuyer utilityFunctionSeller)


