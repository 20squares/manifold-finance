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

-- TODO
-- We assume the contract as given; there is no need to optimize over the contract in the first step; can be changed later.
-- NOTE We ignore blockinformation for now; this seems like something that can be abstracted away in the concerns made in this paper here
-- NOTE We assume that the main relevance for manifold is to decide the seller decision; no need to extensively optimize over buyer decision to choose a contract; we need to think this through
-- NOTE The utility assumption on the player might require private information; need to think about this as well
-- Check the exact deliverable format
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
recoupLHBuyer buyerName sellerName = [opengame|

   inputs    : tx, pi, contract;
   feedback  : ;

   :----------------------------:
   inputs    : tx, pi, contract ;
   feedback  : ;
   operation : dependentDecision buyerName (const [Refund,Forfeit]);
   outputs   : recoupDecision ;
   returns   : recoupLHPayoffBuyer contract pi recoupDecision ;

   inputs    : tx, pi;
   feedback  : ;
   operation : noLHBuyer buyerName sellerName ;
   outputs   : ;
   returns   : ;
   // NOTE the game above mirrors the decision in the noLHBuyer case

   :----------------------------:

   outputs   : ;
   returns   : ;
  |]

-- | Buyer publish TX
-- TODO reuse noLHBuyer from above?
publishLHBuyer buyerName = [opengame|

   inputs    : tx, pi, contract ;
   feedback  : ;

   :----------------------------:
   inputs    : pi ;
   feedback  : ;
   operation : dependentDecision buyerName (const [Publish,NoOp]);
   outputs   : publishDecision ;
   returns   : 0 ;
   // No Payoffs at this stage

   inputs    : publishDecision ;
   feedback  : ;
   operation : forwardFunction transformPublishDecision ;
   outputs   : publishDecisionGame ;
   returns   : ;
   // Translates buyer choice into branching type

   :----------------------------:

   outputs   : publishDecisionGame ;
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

   inputs    : acceptanceDecision ;
   feedback  : ;
   operation : forwardFunction transformAcceptDecision ;
   outputs   : acceptanceDecisionGame ;
   returns   : ;
   // Translates seller choice into branching type

   :----------------------------:

   outputs   : acceptanceDecisionGame ;
   returns   : ;
  |]

-- | fulfill LH seller if published
fulfillLHSellerPublished buyerName sellerName = [opengame|

   inputs    : contract,tx,pi ;
   feedback  : ;

   :----------------------------:
   inputs    : contract,tx, pi ;
   feedback  : ;
   operation : dependentDecision sellerName (const [Confirm, Exhaust, Ignore]);
   outputs   : fulfillDecision ;
   returns   : fulfillLHPayoffSeller contract tx pi fulfillDecision ;

   inputs    : contract,tx, pi, fulfillDecision ;
   feedback  : ;
   operation : forwardFunction $ fulfillLHPayoffBuyer ;
   outputs   : payoffBuyer ;
   returns   : ;
   // Compute payoffs for buyer

   inputs    : payoffBuyer ;
   feedback  : ;
   operation : addPayoffs buyerName ;
   outputs   : ;
   returns   : ;
   // Book-keeping for buyer's payoffs


   :----------------------------:

   outputs   : fulfillDecision ;
   returns   : ;
  |]


-- | fulfill LH seller if no-op
fulfillLHSellerNoOp buyerName sellerName = [opengame|

   inputs    : contract,tx,pi ;
   feedback  : ;

   :----------------------------:
   inputs    : contract,tx,pi ;
   feedback  : ;
   operation : dependentDecision sellerName (const [Exhaust, Ignore]);
   outputs   : fulfillDecision ;
   returns   : fulfillLHPayoffSeller contract tx pi fulfillDecision ;
   // NOTE: we restrict the strategy space - the rest of the game is the same as before

   inputs    : contract,tx, pi, fulfillDecision ;
   feedback  : ;
   operation : forwardFunction $ fulfillLHPayoffBuyer ;
   outputs   : payoffBuyer ;
   returns   : ;
   // Compute payoffs for buyer

   inputs    : payoffBuyer ;
   feedback  : ;
   operation : addPayoffs buyerName ;
   outputs   : ;
   returns   : ;
   // Book-keeping for buyer's payoffs


   :----------------------------:

   outputs   : fulfillDecision ;
   returns   : ;
  |]



