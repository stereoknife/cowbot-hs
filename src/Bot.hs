{-# LANGUAGE OverloadedStrings #-}

module Bot where

import           Control.Monad        (forM_, when)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

import           UnliftIO             (liftIO)
import           UnliftIO.Concurrent  (threadDelay)

import           Discord              (DiscordHandler, RunDiscordOpts (discordOnEnd, discordOnEvent, discordOnLog, discordOnStart, discordToken),
                                       def, restCall, runDiscord)
import qualified Discord.Requests     as R
import           Discord.Types        (Channel (ChannelText, channelId),
                                       Event (MessageCreate), Guild (guildId),
                                       Message (messageAuthor, messageText),
                                       PartialGuild (partialGuildId),
                                       User (userIsBot), messageChannel)

import           Commands             (commandSwitch, runCommand)
import           Control.Monad.Reader (runReader)
import           Control.Monad.State  (runState)
import           Parser               (Parser (..), prefix)
import           Secrets              (token)

import           Debug.Trace          (trace)

pingpongExample :: IO ()
pingpongExample = do
  tok <- token
  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <- runDiscord $ def { discordToken = tok
                        , discordOnStart = liftIO $ putStrLn "Started"
                        , discordOnEnd = liftIO $ putStrLn "Ended"
                        , discordOnEvent = eventHandler
                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                        }
  threadDelay (1 `div` 10 * 10^(6 :: Int))
  TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandler ()
startHandler = do
  Right partialGuilds <- restCall R.GetCurrentUserGuilds

  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall $ R.GetGuildChannels (guildId guild)
    case filter isTextChannel chans of
      (c:_) -> do _ <- restCall $ R.CreateMessage (channelId c) "Hello! I will reply to pings with pongs"
                  pure ()
      _ -> pure ()

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m ->
        when (not $ fromBot m) $
          let r = runParser prefix $ messageText m
          in case r of Just (px, rest) -> do -- runCommand commandSwitch m rest
                                          runCommand commandSwitch m rest
                                          pure ()
                       _               -> liftIO $ print "boot"

      -- MessageReactionAdd r -> reactionSwitch r
      _ -> pure ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _                = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor
