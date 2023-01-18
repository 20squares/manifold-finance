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
publishSubgame buyerName sellerName distribution = [opengame|

   inputs    : tx, contract ;
   feedback  : ;

   :----------------------------:
   inputs    : tx, contract ;
   feedback  : ;
   operation : publishLHBuyerRandom buyerName distribution ;
   outputs   : publishDecisionGame ;
   returns   :  ;

   inputs    : publishDecisionGame ;
   feedback  : ;
   operation : publishBranching buyerName sellerName ;
   outputs   : fulfillDecision ;
   returns   :  ;

   :----------------------------:

   outputs   :  ;
   returns   : ;
  |]
 where publishBranching buyerName sellerName  = (fulfillLHSellerPublished buyerName sellerName) +++ (fulfillLHSellerNoOp buyerName sellerName)

-- | Initiate ~> Accepted
acceptSubgame buyerName sellerName distribution = [opengame|

   inputs    : tx, contract,piInit ;
   feedback  : ;

   :----------------------------:
   inputs    : tx, contract,piInit ;
   feedback  : ;
   operation : acceptLHSeller sellerName;
   outputs   : acceptanceDecisionGame ;
   returns   :  ;

   inputs    : acceptanceDecisionGame ;
   feedback  : ;
   operation : acceptBranching buyerName sellerName distribution;
   outputs   : discard;
   returns   : ;

   :----------------------------:

   outputs   : ;
   returns   : ;
  |]
 where acceptBranching  buyerName sellerName distribution = (recoupLHBuyerRandom buyerName sellerName distribution) +++ (publishSubgame buyerName sellerName distribution)

