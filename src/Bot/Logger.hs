{-# LANGUAGE OverloadedStrings #-}

{- |
    Module     : Bot.Logger
    Author     : Andrea Filippi
    Copyright  : (c) Andrea Filippi, 2026
    License    : MIT
    Maintainer : Andrea Filippi <andrea.filippi02@outlook.i>
-}
module Bot.Logger (logDebug, logInfo, logError) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Bot.Types (BotEnv(..))

-- | Prints only if BOT_DEBUG=true
logDebug :: MonadIO m => BotEnv -> Text -> m ()
logDebug env msg = liftIO $ when (isDebug env) $ do
    TIO.putStrLn $ "[DEBUG] " <> msg

-- | Prints always
logInfo :: MonadIO m => Text -> m ()
logInfo msg = liftIO $ TIO.putStrLn $ "[INFO]  " <> msg

-- | Prints always
logError :: MonadIO m => Text -> m ()
logError msg = liftIO $ TIO.putStrLn $ "[ERROR] " <> msg