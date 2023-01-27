{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module TestGame where


import OpenGames.Engine.Engine
import OpenGames.Preprocessor

import Diagnostics

distTest = uniformDist [1]
distTest2 = uniformDist [1,2]



testUtility (nature, choice1, choice2) = (x1,x2)
 where
   x1 = - (nature - choice1)**2
   x2 = - (nature - choice2)**2

testUtility2 (nature, choice1, choice2) = (x1,x2)
 where
   x1 = (- (nature - choice1)**2) 
   x2 = if nature == 2
           then (- (nature - choice2)**2) + 2
           else (- (nature - choice2)**2)



testGame  = [opengame|

   inputs    : ;
   feedback  : ;

   :----------------------------:
   inputs    : ;
   feedback  : ;
   operation : dependentDecision "buyer" $ const [1,2]  ;
   outputs   : decision1 ;
   returns   : utilityBuyer1 ;

   inputs    : ;
   feedback  : ;
   operation : nature distTest ;
   outputs   : pi ;
   returns   : ;

   inputs    : pi;
   feedback  : ;
   operation : dependentDecision "buyer2" $ const [1,2]  ;
   outputs   : decision2 ;
   returns   : utilityBuyer2 ;

   inputs    : pi, decision1, decision2;
   feedback  : ;
   operation : forwardFunction testUtility ;
   outputs   : (utilityBuyer1,utilityBuyer2);
   returns   :  ;



   :----------------------------:

   outputs   :  ;
   returns   :          ;
  |]


eq strat = generateOutput $ evaluate testGame strat void

strategy1
  :: Kleisli Stochastic () Double
strategy1 = pureAction 2

strategy2
  :: Kleisli Stochastic b b
strategy2 = Kleisli (\x -> playDeterministically x)

transformGame 1.0 = Left 1.0
transformGame 2.0 = Right 2.0

strategyTuple = strategy1 ::- strategy2 ::- Nil

testGame2  = [opengame|

   inputs    : ;
   feedback  : ;

   :----------------------------:
   inputs    : ;
   feedback  : ;
   operation : dependentDecision "buyer2" $ const [1,2]  ;
   outputs   : decision1 ;
   returns   : 0 ;

   inputs    : decision1  ;
   feedback  : ;
   operation : forwardFunction transformGame ;
   outputs   : game ;
   returns   : ;



   inputs    : game;
   feedback  : utilityBuyer1;
   operation : branchingGame ;
   outputs   : discard;
   returns   : ;

   :----------------------------:

   outputs   :  ;
   returns   :          ;
  |]



testGame3  = [opengame|

   inputs    : decision1 ;
   feedback  : utilityBuyer1;

   :----------------------------:
   inputs    : ;
   feedback  : ;
   operation : nature distTest ;
   outputs   : pi ;
   returns   : ;

   inputs    : pi;
   feedback  : ;
   operation : dependentDecision "buyer2" $ const [1,2]  ;
   outputs   : decision2 ;
   returns   : utilityBuyer2 ;

   inputs    : pi, decision1, decision2;
   feedback  : ;
   operation : forwardFunction testUtility ;
   outputs   : (utilityBuyer1,utilityBuyer2);
   returns   :  ;



   :----------------------------:

   outputs   :  ;
   returns   :          ;
  |]


testGame4  = [opengame|

   inputs    : decision1 ;
   feedback  : utilityBuyer1;

   :----------------------------:
   inputs    : ;
   feedback  : ;
   operation : nature distTest2 ;
   outputs   : pi ;
   returns   : ;

   inputs    : pi;
   feedback  : ;
   operation : dependentDecision "buyer2" $ const [1,2]  ;
   outputs   : decision2 ;
   returns   : utilityBuyer2 ;

   inputs    : pi, decision1, decision2;
   feedback  : ;
   operation : forwardFunction testUtility2 ;
   outputs   : (utilityBuyer1,utilityBuyer2);
   returns   :  ;



   :----------------------------:

   outputs   :  ;
   returns   :  ;
  |]


branchingGame = testGame3 +++ testGame4

strategyTuple2 = strategy1 ::- strategy2 ::- strategy2 ::- Nil


eq2 strat = do
  let buyer1 ::- buyer2 ::- seller3 ::- Nil = evaluate testGame2 strat void
  putStrLn "Initial Decision by Buyer:"
  putStrLn $ showDiagnosticInfoL buyer1
  putStrLn "NoLH Publish Decision by Buyer:"
  putStrLn $ showDiagnosticInfoMaybeL buyer2
  putStrLn "Accept Decision by Seller:"
  putStrLn $ showDiagnosticInfoMaybeL seller3
  putStrLn "Recoup Decision by Buyer:"
