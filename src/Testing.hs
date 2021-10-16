{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Testing where

import Discord.Types (Message(messageText), ReactionInfo)
import Discord
import Data.Text ( Text )
import Control.Monad.Trans.Reader (runReader, asks, ReaderT (runReaderT))
import Data.Kind (Type)

mycommand :: Command ()
mycommand = command $ do
    txt <- asks messageText
    reply txt

type Wrapper a = ReaderT a DiscordHandler

command :: Wrapper Message a -> Command a
command rm = MkCommand $ \m -> runReaderT rm m

-- implement a transform to keep reply functionality

instance Reply (Wrapper Message) where
    reply = undefined

----

class Reply (m :: Type -> Type) where
    reply :: Text -> m ()

instance Reply DiscordHandler where
    reply t = undefined 

class Parse m where
    parse :: Parser a -> m (Maybe a)

-- Parse should return maybe, otherwise it would fail the whole command should an optional parse fail

-- DiscordHandler on its own has nothing to source the parse from
-- it needs more context

newtype Action a = MkAction { runAction :: Text -> DiscordHandler a }
newtype Reaction a = MkReaction { runReaction :: ReactionInfo -> DiscordHandler a }
newtype Command a = MkCommand { runCommand :: Message -> DiscordHandler a }

-- now I can add Parse to Command and tell it what to fetch

instance Parse Command where
    parse p = MkCommand $ \m -> do
        let t = messageText m
            a = runParser p t
        pure $ fst <$> a

{- This one is more complicated
instance Parse Reaction where
    parse p = MkCommand $ \r -> do
        let t = messageText m
            a = runParser p t
        pure $ fst <$> a
--}

instance Parse Action where
    parse p = MkAction $ \t -> pure $ fst <$> runParser p t

-- !!!!! these parse instances are incomplete  !!!!! --

-- Missing!: Action should be a generic form of Reaction & Command.
--           Could use exposes/context, actionable/convert to action, etc.


newtype Parser a = MkParser { runParser :: Text -> Maybe (a, Text) }