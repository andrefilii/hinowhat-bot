{-# LANGUAGE OverloadedStrings #-}

{- |
    Module     : Bot.LogicSpec
    Author     : Andrea Filippi
    Copyright  : (c) Andrea Filippi, 2026
    License    : MIT
    Maintainer : Andrea Filippi <andrea.filippi02@outlook.i>
-}
module Bot.PersistenceSpec (spec) where

import Test.Hspec
import Bot.Persistence (fetchListWith)
import Bot.Types (PoolCategory(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (isSuffixOf)

-- | MOCK
-- This function simulates the File System: it receives a path and returns ByteString content, just like B.readFile.
mockReader :: FilePath -> IO B.ByteString
mockReader path
    -- Mocking the Greeting File
    | "greetings/test.json" `isSuffixOf` path = return $ BS.pack
        "{ \"generic\": [\"Hello Test\", \"Hi Test\"], \"with_name\": [\"Hello {name}\"] }"
    
    -- Mocking the No File
    | "no/test.json" `isSuffixOf` path = return $ BS.pack
        "{ \"answers\": [\"No Way\", \"Nope\"] }"

    | "special/test.json" `isSuffixOf` path = return $ BS.pack
        "{\"sentences\": [\"Easter Egg\", \"You Found Me!\"]}"
    
    -- Default: File not found (simulated empty/bad json)
    | otherwise = return ""

spec :: Spec
spec = do
  describe "Bot.Persistence" $ do
    
    describe "fetchListWith (Mocked IO)" $ do
      
      it "correctly extracts Generic Greetings from the JSON" $ do
        -- We inject 'mockReader' into the function
        items <- fetchListWith mockReader GreetingSimple "test"
        items `shouldBe` ["Hello Test", "Hi Test"]

      it "correctly extracts Named Greetings from the SAME JSON file" $ do
        items <- fetchListWith mockReader GreetingNamed "test"
        items `shouldBe` ["Hello {name}"]

      it "correctly extracts No Answers from the No JSON file" $ do
        items <- fetchListWith mockReader NoAnswer "test"
        items `shouldBe` ["No Way", "Nope"]

      it "correctly extracts Special Sentences from the Special JSON file" $ do
        items <- fetchListWith mockReader Special "test"
        items `shouldBe` ["Easter Egg", "You Found Me!"]

      it "returns empty list if file content is invalid/empty" $ do
        items <- fetchListWith mockReader GreetingSimple "nonexistent"
        items `shouldBe` []