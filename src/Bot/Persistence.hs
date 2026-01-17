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
      -- !! Exposed just for testing
    , fetchListWith 
    ) where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Text (Text)
import System.FilePath ((</>))

import Bot.Types
import Bot.Constants (dataDir)

-- | PRIVATE: Maps the category to the folder name on disk.
getFolderName :: PoolCategory -> String
getFolderName GreetingSimple = "greetings"
getFolderName GreetingNamed  = "greetings"
getFolderName NoAnswer       = "no"
getFolderName Special        = "special"

fetchListWith :: (FilePath -> IO B.ByteString) -> PoolCategory -> Text -> IO [Text]
fetchListWith reader category langText = do
    let lang = T.unpack langText
    let folder = getFolderName category
    let path = dataDir </> folder </> (lang ++ ".json")

    -- TODO manage exception. For now if file missing return empty list
    content <- reader path

    -- Decode and Extract based on Category
    case category of
        GreetingSimple -> 
            return $ case decode content of
                Just (GreetingPool g _) -> g  -- returns only the generic list
                Nothing                 -> []
        
        GreetingNamed -> 
            return $ case decode content of
                Just (GreetingPool _ n) -> n  -- returns only the named list
                Nothing                 -> []

        NoAnswer -> 
            return $ case decode content of
                Just (NoPool ans) -> ans
                Nothing           -> []

        Special -> 
            return $ case decode content of
                Just (SpecialPool s) -> s
                Nothing              -> []

fetchList :: PoolCategory -> Text -> IO [Text]
fetchList = fetchListWith B.readFile

-- | Get the number of items in a specific pool
getPoolSize :: PoolCategory -> Text -> IO Int
getPoolSize category lang = do
    items <- fetchList category lang
    return (length items)

-- | Get a specific item by index from a pool.
getPoolItem :: PoolCategory -> Text -> Int -> IO (Maybe Text)
getPoolItem category lang index = do
    items <- fetchList category lang
    if index >= 0 && index < length items
        then return (Just (items !! index))
        else return Nothing