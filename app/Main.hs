{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (exitFailure)

import Bot.Types (BotEnv(..))
import Bot.Discord (runBot)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    putStrLn "--- Hinowhat Bot Starting ---"

    maybeToken <- lookupEnv "DISCORD_TOKEN"
    envToken <- case maybeToken of
        Just t -> return (T.pack t)
        Nothing -> do
            putStrLn "ERROR: Environment variable 'DISCORD_TOKEN' is missing!"
            exitFailure

    maybeSecret <- lookupEnv "BOT_SECRET"
    envSecret <- case maybeSecret of
        Just s -> return (T.pack s)
        Nothing -> do
            putStrLn "WARNING: 'BOT_SECRET' not set. Using default insecure secret."
            return "default_insecure_secret"

    maybeLang <- lookupEnv "BOT_LANGUAGE"
    let lang = T.pack (fromMaybe "it" maybeLang)

    let env = BotEnv
          { staticSeed      = envSecret
          , defaultLanguage = lang
          , token           = envToken
          }

    putStrLn $ "Configuration loaded. Language: " ++ T.unpack lang

    runBot env