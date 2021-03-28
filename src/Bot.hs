module Bot (runBot) where

import           Commands            (about, bless, clap, translate, uwu, yt)
import           Commands.Uwu        (uwuR)
import           Control.Monad       (forM_, unless)
import           Data.Bot            (command, interpret, reaction)
import           Data.Data           (Data (dataTypeOf))
import           Data.Data.Random    (randomValue)
import           Data.Discord        (Exposes (asksExposed))
import           Data.Language       (Lang (English))
import qualified Data.Text.IO        as TIO
import           Data.Text.Lazy      (fromStrict)
import qualified Database.Redis      as DB
import           Discord             (DiscordHandler,
                                      RunDiscordOpts (discordOnEnd, discordOnEvent, discordOnLog, discordOnStart, discordToken),
                                      def, restCall, runDiscord)
import qualified Discord.Requests    as R
import           Discord.Types       (Channel (ChannelText, channelId),
                                      Event (MessageCreate, MessageReactionAdd),
                                      Guild (guildId),
                                      Message (messageAuthor, messageText),
                                      PartialGuild (partialGuildId),
                                      User (userIsBot), emojiName,
                                      reactionEmoji)
import           Parser              (Parser (runParser))
import           Parser.Constructors (string, word, ws)
import           Secrets             (botAdmins, token)
import           UnliftIO            (MonadIO (liftIO))
import           UnliftIO.Concurrent (threadDelay)

runBot :: IO ()
runBot = do
  tok <- token
  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <- runDiscord $ def { discordToken = tok
                        , discordOnStart = startHandler
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
  admins <- liftIO botAdmins

  forM_ admins $ \admin -> do
    Right c <- restCall $ R.CreateDM admin
    restCall $ R.CreateMessage (channelId c) "Bot is up"

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m ->
        unless (fromBot m) $ do
          let mt = fromStrict $ messageText m
              ps = runParser (ws $ string "🤠" >> word) mt
          interpret (maybe "" fst ps) (m, maybe "" snd ps) $ do
            command "about" about
            command "bless" bless
            command "t" $ translate English $ maybe "" snd ps
            command "clap" clap
            command "yt" yt
            command "uwu" uwu

      MessageReactionAdd r -> interpret (fromStrict $ emojiName $ reactionEmoji r) r $ do
          reaction "♋" uwuR
          reaction "🔣" $ translate English =<< asksExposed (fromStrict . messageText)
          reaction "🗺️" $ do
            l <- liftIO $ randomValue $ dataTypeOf English
            t <- asksExposed (fromStrict . messageText)
            translate l t
      _ -> pure ()

isTextChannel :: Channel -> Bool
isTextChannel ChannelText {} = True
isTextChannel _              = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor
