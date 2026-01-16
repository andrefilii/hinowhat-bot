{-# LANGUAGE OverloadedStrings #-}

{- |
    Module     : Bot.LogicSpec
    Author     : Andrea Filippi
    Copyright  : (c) Andrea Filippi, 2026
    License    : MIT
    Maintainer : Andrea Filippi <andrea.filippi02@outlook.i>
    
This module verifies that the RNG logic is deterministic, handles edge cases correctly, and responds to time changes.
-}
module Bot.LogicSpec (spec) where

import Test.Hspec
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Bot.Logic (pickRandomIndex, shouldTriggerSpecial)

-- | Fixed dateTime (2026-01-01T12:00:00) for deterministic testing.
fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 43200)

spec :: Spec
spec = do
  describe "Bot.Logic" $ do
    
    -- === TEST: INDEX PICKER
    describe "pickRandomIndex" $ do

      context "when handling edge cases" $ do
        it "returns Nothing if the pool size is 0" $ do
          pickRandomIndex "secret" fixedTime 0 `shouldBe` Nothing

        it "returns Nothing if the pool size is negative" $ do
          pickRandomIndex "secret" fixedTime (-5) `shouldBe` Nothing

        it "always returns index 0 if the pool size is 1" $ do
          pickRandomIndex "secret" fixedTime 1 `shouldBe` Just 0

      context "when checking determinism (same inputs)" $ do
        it "returns the exact same index for the same secret and time" $ do
          let result1 = pickRandomIndex "mySecretKey" fixedTime 10
          let result2 = pickRandomIndex "mySecretKey" fixedTime 10
          result1 `shouldBe` result2

      context "when checking variance (different inputs)" $ do
        it "returns a different index when the time changes (high probability)" $ do
          -- Create a time 1 second later (2026-01-01T12:00:01)
          let time2 = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 43201)
          
          -- With a pool of 100, the chance of collision is 1%.
          -- Ideally, we would mock the RNG, but testing variance is sufficient here.
          let result1 = pickRandomIndex "key" fixedTime 100
          let result2 = pickRandomIndex "key" time2 100
          
          result1 `shouldNotBe` result2

        it "returns a different index when the secret changes" $ do
          let result1 = pickRandomIndex "keyA" fixedTime 100
          let result2 = pickRandomIndex "keyB" fixedTime 100
          result1 `shouldNotBe` result2

    -- TEST: SPECIAL EVENT
    describe "shouldTriggerSpecial" $ do
      
      it "is deterministic based on time and secret" $ do
        let res1 = shouldTriggerSpecial "secret" fixedTime
        let res2 = shouldTriggerSpecial "secret" fixedTime
        res1 `shouldBe` res2