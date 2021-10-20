{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Howdy.Bot where
import Howdy.Actions ( Action(..))
import Discord
    ( runDiscord,
      DiscordHandler,
      RunDiscordOpts(discordToken, discordOnEvent),
      def )
import Data.Default ( Default(..) )
import Control.Monad.Writer
    ( when, MonadWriter(tell), Writer, WriterT(WriterT) )
import Data.Text ( Text )
import Discord.Types ( Emoji, Event(MessageCreate), Message (messageText) )
import UnliftIO ( Chan, newChan )
import qualified Data.Text.IO as TIO
import Control.Monad.Reader ( ReaderT(runReaderT), when, void )
import Control.Applicative
import Howdy.Parser (string, Parser (runParser), firstof, word)


data BotData = BotData { prefixes :: [Text]
                       , commands :: [Action]
                       , reactions :: [Action] -- this one is to prevent ghc complaining abt data instead of newtype
                       }

instance Default BotData where
    def = BotData mempty mempty mempty

instance Semigroup BotData where
    a <> b = BotData { prefixes = prefixes a <> prefixes b
                     , commands = commands a <> commands b
                     , reactions = reactions a <> reactions b
                     }

instance Monoid BotData where
    mempty = def

newtype Bot a = MkBot { unpackBot :: Writer BotData a }
    deriving (Functor, Applicative, Monad, MonadWriter BotData)

prefix :: [Text] -> Bot ()
prefix pf = tell $ def { prefixes = pf }

command :: Text -> Text -> DiscordHandler () -> Bot ()
command alias desc action = tell $ def { commands = [Command alias desc action] }

reaction :: Emoji -> Text -> DiscordHandler () -> Bot ()
reaction emoji desc action = tell $ def { reactions = [Reaction emoji desc action] }

eventHandler :: BotData -> Event -> DiscordHandler ()
eventHandler bot (MessageCreate m) = go $ do
    firstof string $ prefixes bot
    alias <- word
    runCommands alias $ commands bot
    pure ()

    where parsed p = runParser p $ messageText m
          go p' = seq (parsed p') (pure ())

eventHandler _ _ = pure ()

-- TODO: Make action mcmonad so it can have the nice cool data it needs


runCommand :: Text -> (Action :: Command) -> DiscordHandler ()
runCommand t (Command a _ c) = guard (t == a) c

runCommands :: Text -> [Action :: Command] -> DiscordHandler ()
runCommands t = foldr ((<|>) . runCommand) empty

