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
initLHBuyer  = [opengame|

   inputs    : tx,contract,piOld ;
   feedback  : ;

   :----------------------------:
   inputs    : tx,contract,piOld ;
   feedback  : ;
   operation : dependentDecision "buyer initiate contract" $ actionSpaceInitLHBuyer;
   outputs   : contractDecision ;
   returns   : utilityBuyer ;

   inputs    : contractDecision, tx, piOld ;
   feedback  : ;
   operation : forwardFunction transformInitiateDecision ;
   outputs   : contractDecisionGame ;
   returns   : ;
   // Translates buyer choice into branching type

   :----------------------------:

   outputs   : contractDecisionGame ;
   returns   : utilityBuyer         ;
  |]


-- | Buyer decision whether no LH
noLHBuyer  wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx, piNew ;
   feedback  : utilityBuyer;

   :----------------------------:
   inputs    : tx, piNew ;
   feedback  : ;
   operation : dependentDecision "buyer noLH" (const [NoOp, Publish 0]);
   // NOTE ignore the gasPub variable  as not needed
   outputs   : publishDecision ;
   returns   : utilityBuyer ;

   inputs    : tx, piNew, publishDecision ;
   feedback  : ;
   operation : forwardFunction $ utilityFBuyer  ;
   outputs   : utilityBuyer ;
   returns   : ;
   // Compute utility for buyer

   inputs    : tx, piNew ;
   feedback  : ;
   operation : forwardFunction $ utilityFSeller  ;
   outputs   : utilitySeller ;
   returns   : ;
   // Compute utility for seller
   :----------------------------:

   outputs   : ;
   returns   : ;
  |]
 where
   utilityFBuyer x = utilityFunctionBuyer $ noLHPayoffBuyer wealthBuyer  x
   utilityFSeller x = utilityFunctionSeller $ (uncurry $ noLHPayoffSeller wealthSeller) x


-- | Buyer decision whether no LH with random sample
noLHBuyerRandom  wealthBuyer wealthSeller distribution utilityFunctionBuyer utilityFunctionSeller= [opengame|

   inputs    : tx ;
   feedback  : utilityBuyer;

   :----------------------------:
   inputs    : ;
   feedback  : ;
   operation : gasPriceDistribution distribution;
   outputs   : piNew ;
   returns   : ;

   inputs    : tx, piNew ;
   feedback  : utilityBuyer;
   operation : noLHBuyer  wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller ;
   outputs   : ;
   returns   : ;

   :----------------------------:

   outputs   : ;
   returns   : ;
  |]

-- | Buyer recoup LH
recoupLHBuyer  wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx, contract, piNew, piOld;
   feedback  : utilityBuyer, utilitySeller;

   :----------------------------:
   inputs    : tx, contract, piNew, piOld ;
   feedback  : ;
   operation : dependentDecision "recoup LH" (const [Refund,Forfeit]);
   outputs   : recoupDecision ;
   returns   : utilityBuyer ;
   // NOTE we make a shortcut for the buyer's decision to run the tx

   inputs    : tx, contract, piNew, piOld, recoupDecision ;
   feedback  : ;
   operation : forwardFunction $ utilityFBuyer ;
   outputs   : utilityBuyer ;
   returns   : ;
   // Compute payoffs for seller

   inputs    : tx, piNew ;
   feedback  : ;
   operation : forwardFunction $ utilityFSeller ;
   outputs   : utilitySeller ;
   returns   : ;
   // Compute payoffs for seller

   :----------------------------:

   outputs   : ;
   returns   : ;
  |]
  where
    utilityFBuyer x = utilityFunctionBuyer $ recoupLHPayoffBuyer wealthBuyer x
    utilityFSeller x = utilityFunctionSeller $ (uncurry $ recoupLHPayoffSeller wealthSeller) x


-- | Buyer recoup LH with random sample
recoupLHBuyerRandom  wealthBuyer wealthSeller distribution utilityFunctionBuyer utilityFunctionSeller= [opengame|

   inputs    : tx, contract, piOld ;
   feedback  : utilityBuyer, utilitySeller ;

   :----------------------------:

   inputs    : ;
   feedback  : ;
   operation : gasPriceDistribution distribution;
   outputs   : piNew ;
   returns   : ;

   inputs    : tx, contract, piNew, piOld ;
   feedback  : utilityBuyer, utilitySeller ;
   operation : recoupLHBuyer  wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller;
   outputs   :  ;
   returns   :  ;

   :----------------------------:

   outputs   : ;
   returns   : ;
  |]



-- | Buyer publish TX
-- TODO reuse noLHBuyer from above?
publishLHBuyer  possibleGasPubLS= [opengame|

   inputs    : tx, contract, piNew, piOld  ;
   feedback  : ;

   :----------------------------:
   inputs    : piNew ;
   feedback  : ;
   operation : dependentDecision "buyer publish LH1" (const [NoOp, Publish 0]);
   // This is a hack to refine choice of gPub in the next decision
   outputs   : publishDecision ;
   returns   : utilityBuyer ;
   // No Payoffs at this stage

   inputs    : piNew,tx,publishDecision ;
   feedback  : ;
   operation : dependentDecision "buyer publish LH2" $ actionSpacePublishLHBuyer possibleGasPubLS ;
   outputs   : publishDecisionRefined ;
   returns   : utilityBuyer ;
   // No Payoffs at this stage

   inputs    : publishDecisionRefined, (tx, contract, piNew,piOld);
   feedback  : ;
   operation : forwardFunction $ uncurry transformPublishDecision ;
   outputs   : publishDecisionGame ;
   returns   : ;
   // Translates buyer choice into branching type

   :----------------------------:

   outputs   : publishDecisionGame ;
   returns   : utilityBuyer;
  |]


-- | Buyer publish TX with random sample
publishLHBuyerRandom  distribution possibleGasPubLS = [opengame|

   inputs    : tx, contract, piOld ;
   feedback  : utilityBuyer, seller;

   :----------------------------:
   inputs    : ;
   feedback  : ;
   operation : gasPriceDistribution distribution;
   outputs   : piNew ;
   returns   : ;

   inputs    : tx, contract, piNew, piOld ;
   feedback  : ;
   operation : publishLHBuyer  possibleGasPubLS;
   outputs   : publishDecisionGame ;
   returns   : utilityBuyer ;


   :----------------------------:

   outputs   : publishDecisionGame ;
   returns   : utilityBuyer, seller;
  |]
  
-- 1.2. Seller
-- | Accept LH seller
acceptLHSeller  = [opengame|

   inputs    : tx, contract,piOld ;
   feedback  : ;

   :----------------------------:
   inputs    : tx, contract,piOld ;
   feedback  : ;
   operation : dependentDecision "sellerAccept" (const [Decline,Accept]);
   outputs   : acceptanceDecision ;
   returns   : utilitySeller ;

   inputs    : acceptanceDecision, (tx, contract, piOld)  ;
   feedback  : ;
   operation : forwardFunction $ uncurry transformAcceptDecision ;
   outputs   : acceptanceDecisionGame ;
   returns   : ;
   // Translates seller choice into branching type

   :----------------------------:

   outputs   : acceptanceDecisionGame ;
   returns   : utilitySeller;
  |]


-- | fulfill LH seller if published
fulfillLHSellerPublished  wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx,contract,piNew,piOld,gasPub ;
   feedback  : utilityBuyer,utilitySeller;

   :----------------------------:
   inputs    : tx, contract, piNew, piOld, gasPub ;
   feedback  : ;
   operation : dependentDecision "sellerFulfill" (const [Confirm, Exhaust, Ignore]);
   outputs   : fulfillDecision ;
   returns   : utilitySeller ;

   inputs    : tx, contract, gasPub, piNew, piOld, fulfillDecision ;
   feedback  : ;
   operation : forwardFunction $ utilityFSeller ;
   outputs   : utilitySeller ;
   returns   : ;
   // Compute payoffs for seller

   inputs    : tx, contract, piNew, piOld, fulfillDecision ;
   feedback  : ;
   operation : forwardFunction $ utilityFBuyer ;
   outputs   : utilityBuyer ;
   returns   : ;
   // Compute payoffs for buyer
   
   :----------------------------:

   outputs   : fulfillDecision ;
   returns   : ;
  |]
 where
   utilityFBuyer x = utilityFunctionBuyer $ fulfillLHPayoffBuyer wealthBuyer x
   utilityFSeller x = utilityFunctionSeller $ fulfillLHPayoffSeller wealthSeller x


-- | fulfill LH seller if no-op
fulfillLHSellerNoOp  wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller = [opengame|

   inputs    : tx, contract, piNew, piOld ;
   feedback  : utilityBuyer,utilitySeller;

   :----------------------------:
   inputs    : tx, contract, piNew, piOld ;
   feedback  : ;
   operation : dependentDecision "seller NoFulfill" (const [Exhaust, Ignore]);
   outputs   : noFulfillDecision ;
   returns   : utilitySeller ;
   // NOTE: we restrict the strategy space - the rest of the game is the same as before

   inputs    : tx, contract, piNew, piOld, noFulfillDecision ;
   feedback  : ;
   operation : forwardFunction $ utilitySeller ;
   outputs   : utilitySeller ;
   returns   : ;
   // Compute payoffs for seller

   inputs    : tx, contract, piNew, piOld, noFulfillDecision ;
   feedback  : ;
   operation : forwardFunction $ utilityBuyer ;
   outputs   : utilityBuyer ;
   returns   : ;
   // Compute payoffs for buyer

   :----------------------------:

   outputs   : noFulfillDecision ;
   returns   : ;
  |]
 where
   utilityBuyer x = utilityFunctionBuyer $ noFulfillLHPayoffBuyer wealthBuyer x
   utilitySeller x = utilityFunctionSeller $ noFulfillLHPayoffSeller wealthSeller x


