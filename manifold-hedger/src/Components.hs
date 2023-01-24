{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Components where


import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import ActionSpaces
import Payoffs
import Types

{-
This file contains the main model components
-}

-- NOTE We assume the contract as given; there is no need to optimize over the contract in the first step; can be changed later. 
-- NOTE We assume that the main relevance for manifold is to decide the seller decision; no need to extensively optimize over buyer decision to choose a contract; we need to think this through
-- NOTE We ignore blockinformation for now; this seems like something that can be abstracted away in the concerns made in this paper here
-- NOTE The utility assumption on the player might require private information; need to think about this as well
-- TODO Check the exact deliverable format
-- TODO Construct the overall game with an eye on having differential information for the different players: The best way probably would be for the seller to observe something initially which affects the distribution at time T. So that the probability distribution is a stochastic channel.
-- TODO This also needs to be constructed with the possibility of importing information from the outset.
-- TODO Run primitive analysis and verify existing results
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
initLHBuyer buyerName  = [opengame|

   inputs    : tx,contract,piOld ;
   feedback  : ;

   :----------------------------:
   inputs    : tx,contract,piOld ;
   feedback  : ;
   operation : dependentDecision buyerName $ actionSpaceInitLHBuyer;
   outputs   : contractDecision ;
   returns   : 0 ;

   inputs    : contractDecision, tx, piOld ;
   feedback  : ;
   operation : forwardFunction transformInitiateDecision ;
   outputs   : contractDecisionGame ;
   returns   : ;
   // Translates buyer choice into branching type

   :----------------------------:

   outputs   : contractDecisionGame ;
   returns   :          ;
  |]

-- | Buyer decision whether no LH
noLHBuyer buyerName sellerName wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx, piNew ;
   feedback  : ;

   :----------------------------:
   inputs    : tx, piNew ;
   feedback  : ;
   operation : dependentDecision buyerName (const [NoOp, Publish 0]);
   // NOTE ignore the gasPub variable 
   outputs   : publishDecision ;
   returns   : utilityFunctionBuyer $ noLHPayoffBuyer wealthBuyer tx piNew publishDecision ;

   inputs    : tx, piNew ;
   feedback  : ;
   operation : forwardFunction $ utilitySeller  ;
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
 where
   utilitySeller x = utilityFunctionSeller $ (uncurry $ noLHPayoffSeller wealthSeller) x

-- | Buyer decision whether no LH with random sample
noLHBuyerRandom buyerName sellerName wealthBuyer wealthSeller distribution utilityFunctionBuyer utilityFunctionSeller= [opengame|

   inputs    : tx ;
   feedback  : ;

   :----------------------------:
   inputs    : ;
   feedback  : ;
   operation : gasPriceDistribution distribution;
   outputs   : pi ;
   returns   : ;

   inputs    : tx, pi ;
   feedback  : ;
   operation : noLHBuyer buyerName sellerName wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller ;
   outputs   : ;
   returns   : ;

   :----------------------------:

   outputs   : ;
   returns   : ;
  |]

-- | Buyer recoup LH
recoupLHBuyer buyerName sellerName wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx, contract, piNew, piOld;
   feedback  : ;

   :----------------------------:
   inputs    : tx, contract, piNew, piOld ;
   feedback  : ;
   operation : dependentDecision buyerName (const [Refund,Forfeit]);
   outputs   : recoupDecision ;
   returns   : utilityFunctionBuyer $ recoupLHPayoffBuyer wealthBuyer tx contract piNew piOld recoupDecision ;
   // NOTE we make a shortcut for the buyer's decision to run the tx

   inputs    : tx, piNew ;
   feedback  : ;
   operation : forwardFunction $ utilitySeller ;
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
  where
   utilitySeller x = utilityFunctionSeller $ (uncurry $ recoupLHPayoffSeller wealthSeller) x


-- | Buyer recoup LH with random sample
recoupLHBuyerRandom buyerName sellerName wealthBuyer wealthSeller distribution utilityFunctionBuyer utilityFunctionSeller= [opengame|

   inputs    : tx, contract, piOld;
   feedback  : ;

   :----------------------------:

   inputs    : ;
   feedback  : ;
   operation : gasPriceDistribution distribution;
   outputs   : piNew ;
   returns   : ;

   inputs    : tx, contract, piNew, piOld ;
   feedback  : ;
   operation : recoupLHBuyer buyerName sellerName wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller;
   outputs   :  ;
   returns   :  ;

   :----------------------------:

   outputs   : ;
   returns   : ;
  |]


  
-- | Buyer publish TX
-- TODO reuse noLHBuyer from above?
publishLHBuyer buyerName possibleGasPubLS= [opengame|

   inputs    : tx, contract, piOld,piNew  ;
   feedback  : ;

   :----------------------------:
   inputs    : piNew ;
   feedback  : ;
   operation : dependentDecision buyerName (const [NoOp, Publish 0]);
   // This is a hack to refine choice of gPub in the next decision
   outputs   : publishDecision ;
   returns   : 0 ;
   // No Payoffs at this stage

   inputs    : piNew,tx,publishDecision ;
   feedback  : ;
   operation : dependentDecision buyerName $ actionSpacePublishLHBuyer possibleGasPubLS ;
   outputs   : publishDecisionRefined ;
   returns   : 0 ;
   // No Payoffs at this stage

   inputs    : publishDecisionRefined, (tx, contract, piNew,piOld);
   feedback  : ;
   operation : forwardFunction $ uncurry transformPublishDecision ;
   outputs   : publishDecisionGame ;
   returns   : ;
   // Translates buyer choice into branching type

   :----------------------------:

   outputs   : publishDecisionGame ;
   returns   : ;
  |]


-- | Buyer publish TX with random sample
publishLHBuyerRandom buyerName distribution possibleGasPubLS = [opengame|

   inputs    : tx, contract, piOld ;
   feedback  : ;

   :----------------------------:
   inputs    : ;
   feedback  : ;
   operation : gasPriceDistribution distribution;
   outputs   : piNew ;
   returns   : ;

   inputs    : tx, contract, piNew, piOld ;
   feedback  : ;
   operation : publishLHBuyer buyerName possibleGasPubLS;
   outputs   : publishDecisionGame ;
   returns   :  ;


   :----------------------------:

   outputs   : publishDecisionGame ;
   returns   : ;
  |]

-- 1.2. Seller
-- | Accept LH seller
acceptLHSeller sellerName  = [opengame|

   inputs    : tx, contract,piOld ;
   feedback  : ;

   :----------------------------:
   inputs    : tx, contract,piOld ;
   feedback  : ;
   operation : dependentDecision sellerName (const [Decline,Accept]);
   outputs   : acceptanceDecision ;
   returns   : 0 ;

   inputs    : acceptanceDecision, (tx, contract, piOld)  ;
   feedback  : ;
   operation : forwardFunction $ uncurry transformAcceptDecision ;
   outputs   : acceptanceDecisionGame ;
   returns   : ;
   // Translates seller choice into branching type

   :----------------------------:

   outputs   : acceptanceDecisionGame ;
   returns   : ;
  |]

-- | fulfill LH seller if published
fulfillLHSellerPublished buyerName sellerName wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx,contract,piNew,piOld,gasPub ;
   feedback  : ;

   :----------------------------:
   inputs    : tx, contract, piNew, piOld, gasPub ;
   feedback  : ;
   operation : dependentDecision sellerName (const [Confirm, Exhaust, Ignore]);
   outputs   : fulfillDecision ;
   returns   : utilityFunctionSeller $ fulfillLHPayoffSeller wealthSeller tx contract gasPub piNew piOld fulfillDecision ;

   inputs    : tx, contract, piNew, piOld, fulfillDecision ;
   feedback  : ;
   operation : forwardFunction $ utilityBuyer ;
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
 where
   utilityBuyer x = utilityFunctionBuyer $ fulfillLHPayoffBuyer wealthBuyer x


-- | fulfill LH seller if no-op
fulfillLHSellerNoOp buyerName sellerName wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx, contract, piNew, piOld ;
   feedback  : ;

   :----------------------------:
   inputs    : tx, contract, piNew, piOld ;
   feedback  : ;
   operation : dependentDecision sellerName (const [Exhaust, Ignore]);
   outputs   : noFulfillDecision ;
   returns   : utilityFunctionSeller $ noFulfillLHPayoffSeller wealthSeller tx contract piNew piOld  noFulfillDecision ;
   // NOTE: we restrict the strategy space - the rest of the game is the same as before

   inputs    : tx, contract, piNew, piOld, noFulfillDecision ;
   feedback  : ;
   operation : forwardFunction $ utilityBuyer ;
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

   outputs   : noFulfillDecision ;
   returns   : ;
  |]
 where
   utilityBuyer x = utilityFunctionBuyer $ noFulfillLHPayoffBuyer wealthBuyer x


