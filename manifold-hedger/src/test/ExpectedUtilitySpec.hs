{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExpectedUtilitySpec where


import           Payoffs
import           Parameterization
import           Types

import           Numeric.Probability.Distribution (expected)
import           Test.Hspec



spec :: Spec
spec = do
  payoffsSeller
  payoffsSellerPayment
  equilibriumPayoffConditions

recoupPayoffSellerExpected Parameters{..} = do
  price <- distribution
  return $ utilityFunctionSeller $ recoupLHPayoffSeller sellerWealth transaction price

noFulfillLHPayoffSellerExpected Parameters{..} decision = do
  price <- distribution
  return $ utilityFunctionSeller $ noFulfillLHPayoffSeller sellerWealth (transaction, contract, price, 100, decision)

fulfillLHPayoffSellerExpected Parameters{..} decision = do
  price <- distribution
  return $ utilityFunctionSeller $ fulfillLHPayoffSeller sellerWealth (transaction, contract, (gasAllocTX transaction), price, 100, decision)

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
  (normalDistribution 3)
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
  (normalDistribution 2)
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
        (expected $ recoupPayoffSellerExpected parameters)
        (21.109361602962768)
     it "is the no fulfill payoff correct - exhaust" $ do
       shouldBe
        (expected $ noFulfillLHPayoffSellerExpected parameters Exhaust)
        (20.713719838411766)
     it "is the no fulfill payoff correct - ignore" $ do
       shouldBe
        (expected $ noFulfillLHPayoffSellerExpected parameters Ignore)
        (21.109361602962768)
     it "is the fulfill payoff correct - exhaust" $ do
       shouldBe
        (expected $ fulfillLHPayoffSellerExpected parameters Exhaust)
        (20.713719838411766)
     it "is the fulfill payoff correct - ignore" $ do
       shouldBe
        (expected $ fulfillLHPayoffSellerExpected parameters Ignore)
        (21.109361602962768)
     it "is the fulfill payoff correct - confirm" $ do
       shouldBe
        (expected $ fulfillLHPayoffSellerExpected parameters Confirm)
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
         (diff parameters Confirm)
         True
