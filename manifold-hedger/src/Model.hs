{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Model where


import OpenGames.Engine.Engine
import OpenGames.Preprocessor
import Types

{-
This file contains the main model components
-}

-- TODO
-- 1. Check the type of actions
-- 2. Construct the overall game
-- 3. Run primitive analysis and verify existing results
-- 4. Adapt for an inclusion of more refined information from the outside

--------------------
-- 1. Representation
-- 1.1. Buyer

-- | Buyer initial decision
initLHBuyer buyerName = [opengame|

   inputs    : ;
   feedback  : ;

   :----------------------------:
   inputs    : ;  
   feedback  : ;
   operation : dependentDecision buyerName (const [Wait,Initiate]);
   outputs   : contract ;
   returns   : 0 ;
   // FIXME ^^ Check payoffs later

   :----------------------------:

   outputs   : contract ;
   returns   :          ;
  |]

-- | Buyer no LH
noLHBuyer buyerName = [opengame|

   inputs    : pi ;
   feedback  : ;

   :----------------------------:
   inputs    : pi ;
   feedback  : ;
   operation : dependentDecision buyerName (const [Publish, NoOp]);
   outputs   : publishDecision ;
   returns   : 0 ;
   // FIXME ^^ Check payoffs later

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

   inputs    : contractParameter ;
   feedback  : ;

   :----------------------------:
   inputs    : contractParameter ;
   feedback  : ;
   operation : dependentDecision sellerName (const [Decline,Accept]);
   outputs   : acceptanceDecision ;
   returns   : 0 ;
   // FIXME ^^ Check payoffs later

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



