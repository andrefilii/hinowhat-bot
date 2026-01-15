{-# LANGUAGE DeriveGeneric #-}

{- |
   Module: Bot.Types
   Author: Andrea Filippi
   Copyright: (c) Andrea Filippi, 2026
   License: MIT
   Maintainer: Andrea Filippi <andrea.filippi02@outlook.i>

This module defines the core data structures used by the hinowhat-bot Discord bot.
It separates the *Shape* of the data (Pools) from the *Locale* (handled by file loading).
-}
module Bot.Types where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Represents a collection of greeting messages.
-- This structure is language-agnostic. The content depends on which JSON file is loaded.
data GreetingPool = GreetingPool
  { 
    generic :: [Text],    -- ^ Simple greetings (e.g., "Hello!")
    with_name :: [Text]   -- ^ Greetings expecting a placeholder (e.g., "Hello {name}!")
  }
  deriving (Show, Generic)

-- | Represents the pool of "No" answers.
data NoPool = NoPool
  { 
    answers :: [Text]     -- ^ 
  }
  deriving (Show, Generic)

-- | Represents the pool of ""Special"" messages.
data SpecialPool = SpecialPool
  { 
    sentences :: [Text]
  }
  deriving (Show, Generic)

-- To automatically parse the JSON file
instance FromJSON GreetingPool
instance FromJSON NoPool
instance FromJSON SpecialPool

-- | The Runtime Environment of the Bot.
-- This holds configuration and state that persists while the bot is running.
data BotEnv = BotEnv
  { -- | The hashed seed used for the deterministic RNG (Easter Egg logic)
    staticSeed :: Text, 
    
    -- | The default language code (e.g., "it", "en") to use if no server config is found.
    defaultLanguage :: Text
    
    -- TODO: Add serverConfigs map for per-guild localization -> serverConfigs :: Map GuildId Text
  }
  deriving (Show)