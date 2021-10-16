{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Howdy.Actions where
import Data.Text (Text)
import Discord ( DiscordHandler )
import Discord.Types ( Message(messageText), ReactionInfo )
import Testing (Parser (runParser))
import Control.Monad.Reader (ReaderT (runReaderT))

newtype Action a = MkAction { runAction :: Text -> DiscordHandler a }
newtype Reaction a = MkReaction { runReaction :: ReactionInfo -> DiscordHandler a }
newtype Command a = MkCommand { runCommand :: Message -> DiscordHandler a }

type Wrapper a = ReaderT a DiscordHandler

class Reply m where
    reply :: Text -> m ()

instance Reply DiscordHandler where
    reply t = undefined

instance Reply (Wrapper Message) where
    reply = undefined

class Parse m where
    parse :: Parser a -> m (Maybe a)

instance Parse Command where
    parse p = MkCommand $ \m -> do
        let t = messageText m
            a = runParser p t
        pure $ fst <$> a

instance Parse Action where
    parse p = MkAction $ \t -> pure $ fst <$> runParser p t

command :: Wrapper Message a -> Command a
command rm = MkCommand $ \m -> runReaderT rm m