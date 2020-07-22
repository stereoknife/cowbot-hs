{-# LANGUAGE OverloadedStrings #-}

module Src.Bot
( run
) where

import Data.Monoid ((<>), Any, getAny)
import Control.Applicative
import Control.Monad (void, forever)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types

import qualified Src.Constants as C

run :: IO ()
run = do
  tok <- TIO.readFile "./auth-token.secret"

  outChan <- newChan :: IO (Chan String)

  threadId <- forkIO $ forever $ readChan outChan >>= putStrLn

  void $ runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler
                          , discordOnEvent = eventHandler
                          , discordOnEnd = killThread threadId
                          }

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler handle event =
  case event of MessageCreate m -> return ()
                _ -> return ()

startHandler :: DiscordHandle -> IO ()
startHandler _ = putStrLn "Ready!"

isCommand :: Message -> Bool
isCommand m = or $ T.isPrefixOf <$> C.prefixes <*> pure (messageText m)