{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Model where


import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import ActionSpaces
import Payoffs
import Types

{-
This file contains the main model components
-}

-- TODO Plan
-- We assume the contract as given; there is no need to optimize over the contract in the first step; can be changed later.
-- We ignore blockinformation for now; this seems like something that can be abstracted away in the concerns made in this paper here
-- Construct the overall game with an eye on having differential information for the different players
-- Run primitive analysis and verify existing results
-- Adapt for an inclusion of more refined information from the outside

--------------------
-- 1. Representation
-- 1.1. Nature
gasPriceDistribution distribution = [opengame|

   inputs    :  ;
   feedback  : ;

   :----------------------------:
   inputs    : ;
   feedback  : ;
   operation : nature distribution ;
   outputs   : pi ;
   returns   :  ;

   :----------------------------:

   outputs   : pi;
   returns   :   ;
  |]


-- 1.2. Buyer
-- | Buyer initial decision
-- Given the transaction that the player wants to implement, a given hl contract, and a current gas price choose whether to initialize the hl contract or not
initLHBuyer buyerName = [opengame|

   inputs    : tx,contract,piInit ;
   feedback  : ;

   :----------------------------:
   inputs    : tx,contract,piInit ;
   feedback  : ;
   operation : dependentDecision buyerName $ actionSpaceInitLHBuyer;
   outputs   : contractDecision ;
   returns   : initialPayoffBuyer tx contractDecision piInit ;

   inputs    : contractDecision ;
   feedback  : ;
   operation : forwardFunction transformInitiateDecision ;
   outputs   : contractDecisionGame ;
   returns   : ;
   // Translates buyer choice into branching type

   :----------------------------:

   outputs   : tx,contractDecisionGame ;
   returns   :          ;
  |]

-- | Buyer decision whether no LH
noLHBuyer buyerName sellerName = [opengame|

   inputs    : tx, pi ;
   feedback  : ;

   :----------------------------:
   inputs    : tx, pi ;
   feedback  : ;
   operation : dependentDecision buyerName (const [Publish, NoOp]);
   outputs   : publishDecision ;
   returns   : noLHPayoffBuyer tx pi publishDecision ;

   inputs    : tx, pi ;
   feedback  : ;
   operation : forwardFunction $ uncurry $ noLHPayoffSeller ;
   outputs   : payoffSeller ;
   returns   : ;
   // Compute payoffs for seller

   inputs    : payoffSeller ;
   feedback  : ;
   operation : addPayoffs sellerName ;
   outputs   : ;
   returns   : ;
   // Book-keeping for seller's payoffs

   :----------------------------:

   outputs   : ;
   returns   : ;
  |]

-- | Buyer recoup LH
recoupLHBuyer buyerName = [opengame|

   inputs    : pi ;
   feedback  : ;

   :----------------------------:
   inputs    : pi ;
   feedback  : ;
   operation : dependentDecision buyerName (const [Refund,Forfeit]);
   outputs   : recoupDecision ;
   returns   : 0 ;
   // FIXME ^^ Check payoffs later

   :----------------------------:

   outputs   : ;
   returns   : ;
  |]

-- | Buyer publish TX
-- TODO reuse noLHBuyer from above?
publishLHBuyer buyerName = [opengame|

   inputs    : pi ;
   feedback  : ;

   :----------------------------:
   inputs    : pi ;
   feedback  : ;
   operation : dependentDecision buyerName (const [Publish,NoOp]);
   outputs   : publishDecision ;
   returns   : 0 ;
   // FIXME ^^ Check payoffs later

   :----------------------------:

   outputs   : publishDecision ;
   returns   : ;
  |]

-- 1.2. Seller
-- | Accept LH seller
acceptLHSeller sellerName = [opengame|

   inputs    : contract,pi ;
   feedback  : ;

   :----------------------------:
   inputs    : contract,pi ;
   feedback  : ;
   operation : dependentDecision sellerName (const [Decline,Accept]);
   outputs   : acceptanceDecision ;
   returns   : acceptLHPayoffSeller contract pi ;

   :----------------------------:

   outputs   : acceptanceDecision ;
   returns   : ;
  |]

-- | fulfill LH seller if published
fulfillLHSellerPublished sellerName = [opengame|

   inputs    : pi, publishParameter ;
   feedback  : ;

   :----------------------------:
   inputs    : pi, publishParameter ;
   feedback  : ;
   operation : dependentDecision sellerName (const [Confirm, Exhaust, Ignore]);
   outputs   : fulfillDecision ;
   returns   : 0 ;
   // FIXME ^^ Check payoffs later

   :----------------------------:

   outputs   : fulfillDecision ;
   returns   : ;
  |]


-- | fulfill LH seller if no-op
fulfillLHSellerNoOp sellerName = [opengame|

   inputs    : pi, noOpParameter ;
   feedback  : ;

   :----------------------------:
   inputs    : pi, noOpParameter ;
   feedback  : ;
   operation : dependentDecision sellerName (const [Exhaust, Ignore]);
   outputs   : fulfillDecision ;
   returns   : 0 ;
   // FIXME ^^ Check payoffs later

   :----------------------------:

   outputs   : fulfillDecision ;
   returns   : ;
  |]



