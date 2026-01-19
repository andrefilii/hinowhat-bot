{-# LANGUAGE OverloadedStrings #-}

{- |
    Module     : Bot.Types
    Author     : Andrea Filippi
    Copyright  : (c) Andrea Filippi, 2026
    License    : MIT
    Maintainer : Andrea Filippi <andrea.filippi02@outlook.i>
    Description : The Discord Controller
-}
module Bot.Discord (runBot) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import Data.Maybe (fromMaybe)

-- Discord Imports
import           Discord
import           Discord.Types
import qualified Discord.Requests as R

import Bot.Types
import Bot.Logic (pickRandomIndex, shouldTriggerSpecial)
import Bot.Persistence (getPoolSize, getPoolItem)
import Bot.Logger

-- | ENTRY POINT
runBot :: BotEnv -> IO ()
runBot env = do
    userFacingError <- runDiscord $ def
        { discordToken = token env
        , discordOnEvent = eventHandler env
        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        }

    -- This is an unrecoverable error
    TIO.putStrLn userFacingError

eventHandler :: BotEnv -> Event -> DiscordHandler ()
eventHandler env event = case event of
    MessageCreate m -> do
        logDebug env $ "Event: MessageCreate | Author: " <> userName (messageAuthor m)

        unless (fromBot m) $ case extractCommand (messageContent m) of
            Just c -> do
                logInfo $ "Command Triggered: " <> T.pack (show c) <> " | User: " <> userName (messageAuthor m)

                case c of
                    GreetingNamed -> do
                        let name = fromMaybe "Stranger" (extractGreetingName (messageContent m))
                        
                        logDebug env $ "Greeting Logic: Extracted name '" <> name <> "'"

                        let transform = T.replace "{{name}}" name
                        response <- buildResponse env GreetingNamed transform
                        
                        sendAndLog env m response

                    command -> do
                        response <- buildResponse env command id
                        sendAndLog env m response

            Nothing -> 
                logDebug env "Message ignored: No command prefix matched."

    _ -> pure ()

-- | Helper to send the message and log if Discord rejects it (Error handling)
sendAndLog :: BotEnv -> Message -> Text -> DiscordHandler ()
sendAndLog env msg response = do
    result <- restCall (R.CreateMessage (messageChannelId msg) response)
    case result of
        Left err -> 
            logError $ "Discord API Failed: " <> T.pack (show err)
        Right _ -> 
            logDebug env "Response sent successfully to Discord."

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

extractCommand :: Text -> Maybe PoolCategory
extractCommand content
    | isHelloCommand (T.toLower content) =
        case extractGreetingName content of
            Just _ -> Just GreetingNamed
            Nothing -> Just GreetingSimple
    | "h!no" `T.isPrefixOf` T.toLower content = Just NoAnswer
    | otherwise = Nothing
  where
    isHelloCommand t = any (`T.isPrefixOf` t) ["h!hello", "h!hi", "h!ciao"]

extractGreetingName :: Text -> Maybe Text
extractGreetingName message =
    let lowerMessage = T.toLower message
        parameter = T.strip $ case True of
            _ | "h!hello" `T.isPrefixOf` lowerMessage -> T.drop 7 message
              | "h!ciao" `T.isPrefixOf` lowerMessage  -> T.drop 6 message
              | "h!hi" `T.isPrefixOf` lowerMessage    -> T.drop 4 message
              | otherwise                              -> ""
    in if T.null parameter then Nothing else Just parameter

buildResponse :: BotEnv -> PoolCategory -> (Text -> Text) -> DiscordHandler Text
buildResponse env requestCategory formatter = do
    logDebug env $ "Building response. Requested Category: " <> T.pack (show requestCategory)

    now <- liftIO getCurrentTime

    let isSpecial = shouldTriggerSpecial (staticSeed env) now

    when isSpecial $ logInfo "Global Secret triggered! Override command with SPECIAL pool."

    let (category, finalFormatter) =
            if isSpecial
            then (Special, id)
            else (requestCategory, formatter)

    logDebug env $ "Final Category selected: " <> T.pack (show category)

    size <- liftIO $ getPoolSize category (defaultLanguage env)
    
    logDebug env $ "Pool size for '" <> T.pack (show category) <> "': " <> T.pack (show size)

    case pickRandomIndex (staticSeed env) now size of
        Nothing -> do
            logError $ "Pool Empty! Category: " <> T.pack (show category) <> ", Lang: " <> defaultLanguage env
            return "Error: The message pool is empty."
        
        Just index -> do
            logDebug env $ "Random Index selected: " <> T.pack (show index)
            
            maybeItem <- liftIO $ getPoolItem category (defaultLanguage env) index
            
            case maybeItem of
                Nothing -> do
                    logError $ "Data Inconsistency! Index " <> T.pack (show index) <> " not found in pool."
                    return "Error: Could not find the message at that index."
                
                Just item -> do
                    logDebug env "Item retrieved and formatted successfully."
                    return (finalFormatter item)