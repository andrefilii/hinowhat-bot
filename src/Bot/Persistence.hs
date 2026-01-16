{-# LANGUAGE OverloadedStrings #-}

{- |
    Module     : Bot.Types
    Author     : Andrea Filippi
    Copyright  : (c) Andrea Filippi, 2026
    License    : MIT
    Maintainer : Andrea Filippi <andrea.filippi02@outlook.i>
    
This module implements the "Data Access Layer". 
It exposes high-level queries (count, fetch-by-index) that abstract away the underlying storage 
mechanism (currently JSON files).
-}
module Bot.Persistence 
    ( -- Public interface
      getPoolSize
    , getPoolItem
    ) where

import Data.Text (Text)

import Bot.Types

-- | PRIVATE: Maps the category to the folder name on disk.
getFolderName :: PoolCategory -> String
getFolderName GreetingSimple = "greetings"
getFolderName GreetingNamed  = "greetings"
getFolderName NoAnswer       = "no"
getFolderName Special        = "special"

-- | Get the number of items in a specific pool
getPoolSize :: PoolCategory -> Text -> IO Int
getPoolSize category lang = undefined -- TODO

-- | Get a specific item by index from a pool.
getPoolItem :: PoolCategory -> Text -> Int -> IO (Maybe Text)
getPoolItem category lang index = undefined -- TODO