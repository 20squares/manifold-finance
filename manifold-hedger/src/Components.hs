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
noLHBuyer  wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller possibleGasLS = [opengame|

   inputs    : tx, piNew ;
   feedback  : utilityBuyer;

   :----------------------------:
   inputs    : tx, piNew ;
   feedback  : ;
   operation : dependentDecision "buyer noLH" $ actionSpaceNoLHBuyer possibleGasLS ;
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
noLHBuyerRandom  wealthBuyer wealthSeller distribution utilityFunctionBuyer utilityFunctionSeller possibleGasLS = [opengame|

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
   operation : noLHBuyer  wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller possibleGasLS;
   outputs   : ;
   returns   : ;

   :----------------------------:

   outputs   : ;
   returns   : ;
  |]

-- | Buyer recoup LH
recoupLHBuyer  wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller possibleGasLS = [opengame|

   inputs    : tx, contract, piNew, piOld;
   feedback  : utilityBuyer, utilitySeller;

   :----------------------------:

   inputs    : tx, piNew ;
   feedback  : ;
   operation : dependentDecision "buyer noLH - recoup" $ actionSpaceNoLHBuyer possibleGasLS ;
   outputs   : publishDecision ;
   returns   : 0 ;

   inputs    : tx, contract, piNew, piOld;
   feedback  : ;
   operation : dependentDecision "recoup LH" (const [Refund,Forfeit]);
   outputs   : recoupDecision ;
   returns   : utilityBuyer  ;

   inputs    : tx, contract, piNew, piOld, recoupDecision, publishDecision ;
   feedback  : ;
   operation : forwardFunction $ utilityFBuyer ;
   outputs   : utilityBuyer ;
   returns   : ;
   // Compute payoffs for buyer

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
recoupLHBuyerRandom  wealthBuyer wealthSeller distribution utilityFunctionBuyer utilityFunctionSeller possibleGasLS = [opengame|

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
   operation : recoupLHBuyer  wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller possibleGasLS;
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

   inputs    : tx, contract, piNew, piOld, fulfillDecision, gasPub;
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
fulfillLHSellerNoOp  wealthBuyer wealthSeller utilityFunctionBuyer utilityFunctionSeller possibleGaseLS = [opengame|

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

   inputs    : tx, piNew ;
   feedback  : ;
   operation : dependentDecision "buyer no fulfill" $ actionSpaceNoLHBuyer possibleGaseLS ;
   outputs   : publishDecision ;
   returns   : 0 ;

   inputs    : tx, contract, piNew, piOld, noFulfillDecision, publishDecision ;
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


