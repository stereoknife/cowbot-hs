module Bot (runBot) where

import           Commands
import           Control.Monad       (forM_, unless)
import           Data.Bot            (command, interpret, reaction)
import           Data.Data           (Data (dataTypeOf))
import           Data.Data.Random    (randomValue)
import           Data.Discord        (Exposes (asksExposed))
import           Data.Language       (Lang (English))
import qualified Data.Text.IO        as TIO
import           Data.Text.Lazy      (fromStrict)
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
import           Secrets             (token)
import           UnliftIO            (MonadIO (liftIO))
import           UnliftIO.Concurrent (threadDelay)

runBot :: IO ()
runBot = do
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
        unless (fromBot m) $ do
          let mt = fromStrict $ messageText m
              ps = runParser (ws $ string "🤠" >> word) mt
          interpret (maybe "" fst ps) (m, maybe "" snd ps) $ do
            command "bless" bless
            command "t" $ translate English $ maybe "" snd ps
            command "clap" clap
            command "yt" yt

      MessageReactionAdd r -> interpret (fromStrict $ emojiName $ reactionEmoji r) r $ do
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


-- Handler :: Command -> Handler
-- (:&) Command -> Command -> Handler

-- Command ["alias"]

{-}
reactionSwitch :: Reaction ()
reactionSwitch = do
    mid <- askReaction reactionMessageId
    cid <- askReaction reactionChannelId
    em  <- askReaction $ emojiName . reactionEmoji
    amt <- dis $ do
           rl <- restCall $ R.GetReactions (cid, mid) em (2, R.BeforeReaction mid)
           case rl of Right m -> return m
                      _       -> return []

    let is e = T.head e == T.head em
    guard (length amt <= 1)
    guard (is "🔣" || is "🗺")

    dis $ restCall $ R.CreateReaction (cid, mid) em
    if
        | is "🔣"    -> reactTranslate $ Just English
        | is "🗺"    -> reactTranslate Nothing
        | otherwise -> pure ()

    return ()
-}
