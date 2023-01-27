{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExpectedUtilitySpec where


import           Payoffs
import           Parameterization
import           Types

import           OpenGames.Engine.Engine

import           Numeric.Probability.Distribution (expected)
import           Test.Hspec



spec :: Spec
spec = do
  comparePayoffs
--  payoffsSeller
--  payoffsSellerPayment
--  equilibriumPayoffConditions

recoupPayoffSellerExpected Parameters{..} = do
  price <- distribution
  return $ utilityFunctionSeller $ recoupLHPayoffSeller sellerWealth transaction price

recoupPayoffSellerLS Parameters{..} = fmap (\price ->  recoupLHPayoffSeller sellerWealth transaction price) [1..20]


noFulfillLHPayoffSellerExpected Parameters{..} decision = do
  price <- distribution
  return $ utilityFunctionSeller $ noFulfillLHPayoffSeller sellerWealth (transaction, contract, price, 100, decision)

fulfillLHPayoffSellerExpected Parameters{..} decision = do
  price <- distribution
  return $ utilityFunctionSeller $ fulfillLHPayoffSeller sellerWealth (transaction, contract, (gasAllocTX transaction), price, 100, decision)

fulfillLHPayoffSellerList Parameters{..} = fmap (\price ->  fulfillLHPayoffSeller sellerWealth (transaction, contract, (gasAllocTX transaction), price, 100, Confirm)) [1..20]

testContractPayment payment =
  HLContract
      (10**9)
      payment
      1
      (0.1*10**6)
      (75*10**3)
      (20*10**3)

diff parameters decision = payoff > 0
  where
    payoff = (expected $ fulfillLHPayoffSellerExpected parameters decision) - (expected $ recoupPayoffSellerExpected parameters)

-- Parameters with varying payment
parametersPayment payment = Parameters
  "buyer"
  "seller"
  (10**9)
  (10**9)
  (uniformDist [1..199])
  testActionSpaceGasPub
  testTransaction
  (testContractPayment payment)
  100
  logUtility
  logUtility

parametersPayment price = Parameters
  "buyer"
  "seller"
  (10**9)
  (10**9)
  (uniformDist [1..199])
  testActionSpaceGasPub
  testTransaction
  testContract
  price
  logUtility
  logUtility

payoffsSeller = describe
  "payoff seller" $ do
     it "is the recoup payoff correct" $ do
       shouldBe
        (expected $ recoupPayoffSellerExpected (parameters $ uniformDist [1..199]))
        (21.109361602962768)
     it "is the no fulfill payoff correct - exhaust" $ do
       shouldBe
        (expected $ noFulfillLHPayoffSellerExpected (parameters $ uniformDist [1..199]) Exhaust)
        (20.713719838411766)
     it "is the no fulfill payoff correct - ignore" $ do
       shouldBe
        (expected $ noFulfillLHPayoffSellerExpected (parameters $ uniformDist [1..199]) Ignore)
        (21.109361602962768)
     it "is the fulfill payoff correct - exhaust" $ do
       shouldBe
        (expected $ fulfillLHPayoffSellerExpected (parameters $ uniformDist [1..199]) Exhaust)
        (20.713719838411766)
     it "is the fulfill payoff correct - ignore" $ do
       shouldBe
        (expected $ fulfillLHPayoffSellerExpected (parameters $ uniformDist [1..199]) Ignore)
        (21.109361602962768)
     it "is the fulfill payoff correct - confirm" $ do
       shouldBe
        (expected $ fulfillLHPayoffSellerExpected (parameters $ uniformDist [1..199]) Confirm)
        (20.717750757624657)


payoffsSellerPayment = describe
  "payoff seller varies with payment?" $ do
     it "is the fulfill payoff correct - confirm payment 100" $ do
       shouldBe
        (expected $ fulfillLHPayoffSellerExpected (parametersPayment 100) Confirm)
        (20.717750077351447)
     it "is the fulfill payoff correct - confirm payment 200" $ do
       shouldBe
        (expected $ fulfillLHPayoffSellerExpected (parametersPayment 200) Confirm)
        (20.717750077351447)
     it "is the fulfill payoff correct - confirm payment 300" $ do
       shouldBe
        (expected $ fulfillLHPayoffSellerExpected (parametersPayment 300) Confirm)
        (20.717750077351447)


equilibriumPayoffConditions = describe
   "payoff conditions for seller" $ do
      it "is accepting better than not accepting?" $ do
        shouldBe
         (diff (parameters $ uniformDist [1..199]) Confirm)
         True

comparePayoffs = describe
    "compare payoffs for fixed prices" $ do
       it "recoup payoff" $ do
         shouldBe
           (recoupPayoffSellerLS $ params)
           [1..20]
       it "recoup utility" $ do
         shouldBe
           (fmap (sqrt) $ recoupPayoffSellerLS $ params)
           [1..20]
      where
        params = parameters $ uniformDist [1..199]
