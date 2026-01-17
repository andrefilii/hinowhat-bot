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

eventHandler :: BotEnv -> Event -> DiscordHandler()
eventHandler env event =
    case event of
        MessageCreate m ->
            unless (fromBot m) $ case extractCommand (messageContent m) of
                Just c -> case c of
                    GreetingNamed -> do
                        let name = fromMaybe "Stranger" (extractGreetingName (messageContent m))
                        let transform = T.replace "{{name}}" name
                        response <- buildResponse env GreetingNamed transform
                        void $ restCall (R.CreateMessage (messageChannelId m) response)

                    command -> do
                        response <- buildResponse env command id
                        void $ restCall (R.CreateMessage (messageChannelId m) response)

                Nothing -> return ()

        _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

extractCommand :: Text -> Maybe PoolCategory
extractCommand content
    | "/hello" `T.isPrefixOf` T.toLower content =
        case extractGreetingName content of
            Just _ -> Just GreetingNamed
            Nothing -> Just GreetingSimple
    | "/no" `T.isPrefixOf` T.toLower content = Just NoAnswer
    | otherwise = Nothing

extractGreetingName :: Text -> Maybe Text
extractGreetingName message =
    let parameter = T.strip (T.drop 6 (T.toLower message))
    in if T.null parameter then Nothing else Just parameter

buildResponse :: BotEnv -> PoolCategory -> (Text -> Text) -> DiscordHandler Text
buildResponse env requestCategory formatter = do
    -- Current time needed for the random index
    now <- liftIO getCurrentTime

    -- Check if the special pool should be used
    let isSpecial = shouldTriggerSpecial (staticSeed env) now

    let (category, finalFormatter) =
            if isSpecial
            then (Special, id)
            else (requestCategory, formatter)

    size <- liftIO $ getPoolSize category (defaultLanguage env)

    case pickRandomIndex (staticSeed env) now size of
        Nothing -> return "Error: The message pool is empty."
        
        Just index -> do
            maybeItem <- liftIO $ getPoolItem category (defaultLanguage env) index
            
            case maybeItem of
                Nothing -> return "Error: Could not find the message at that index."
                Just item -> return (finalFormatter item)