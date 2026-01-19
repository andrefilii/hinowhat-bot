{-# LANGUAGE OverloadedStrings #-}

{- |
    Module     : Bot.Types
    Author     : Andrea Filippi
    Copyright  : (c) Andrea Filippi, 2026
    License    : MIT
    Maintainer : Andrea Filippi <andrea.filippi02@outlook.i>

This module handles the mathematical and random logic of the bot.
It includes the SHA256 hashing mechanism to combine a static secret with the current timestamp to create deterministic
seeds for random number generation.
-}
module Bot.Logic 
   ( -- Public interface
     shouldTriggerSpecial
   , pickRandomIndex
   ) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS -- To easly convert the secret (chars) into bytes
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Random (mkStdGen, randomR)
import Bot.Constants (specialProbability)

-- | INTERNAL: Combines a static secret (the hash of the seed phrase) with the current time to produce a unique 
-- Integer seed. We use SHA256 to mix the inputs so that slight changes in time produce completely different seeds.
-- NOTE: the staticSecret has no particular utility, it's just for fun as it contains inside jokes, funny phrases or
--       whatever you want to put as an easter egg. Just know that every time that you receive an answer, it's
--       also thanks to that little silly string :)
combineSeed :: Text -> UTCTime -> Int
combineSeed staticSecret currentTime =
  let -- Convert UTC time to seconds (integer)
      -- We use floor to change the seed every second
      timeInt = floor (utcTimeToPOSIXSeconds currentTime) :: Int

      -- Combine secret and time string
      comboString = T.unpack staticSecret ++ show timeInt

      -- Hash the combination to ensure uniform distribution
      digest :: Digest SHA256
      digest = hash (BS.pack comboString)

      -- Fold the hash bytes into a single Int to seed the generator
      -- This is a quick way to turn a ByteString into an Int
      bytes = convert digest :: ByteString
      seed = fromIntegral (B.foldl' (\acc b -> acc + fromIntegral b) (0 :: Integer) bytes) :: Int
   in 
      seed

-- | Determines if the special "Easter Egg" event should trigger.
-- Currently configured for a 15% chance. Will eventually become configurable, if I'll remember.
shouldTriggerSpecial ::
  -- | The static secret seed
  Text ->
  -- | The current time
  UTCTime ->
  -- | True if the event triggers
  Bool
shouldTriggerSpecial secret time =
  let 
      seed = combineSeed secret time
      gen = mkStdGen seed
      -- Roll a die from 1 to 100
      (roll, _) = randomR (1 :: Int, 100) gen
   in 
      roll <= specialProbability -- Defined in Bot.Constants

-- | Decides which index to pick from a collection of given size.
-- Returns 'Nothing' if the list is empty.
pickRandomIndex :: Text -> UTCTime -> Int -> Maybe Int
pickRandomIndex _ _ size | size <= 0 = Nothing
pickRandomIndex secret time size =
  let 
      seed = combineSeed secret time
      gen = mkStdGen seed
      -- Generate a random index betweem 0 and (size -1)
      (idx, _) = randomR (0, size - 1) gen
   in 
      Just idx