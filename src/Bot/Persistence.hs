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
      getGreetingSize
    , getGreetingAtIndex
    ) where

import Data.Text (Text)

-- | Interface Method: Get the number of available items.
-- Allows the Logic layer to pick an index without seeing the data.
getGreetingSize :: Text -> IO Int
getGreetingSize langText = undefined -- TODO

-- | Interface Method: Fetch a specific item by index.
-- Acts like a "SELECT item FROM table OFFSET index LIMIT 1"
getGreetingAtIndex :: Text -> Int -> IO (Maybe Text)
getGreetingAtIndex langText index = undefined -- TODO