{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Data.Bot where

import           Control.Monad        (guard)
import           Control.Monad.Reader (ReaderT, asks, runReaderT)
import           Control.Monad.State  (StateT, evalStateT, gets, lift)
import           Data.Discord         (Exposes (askExposed))
import           Data.Text.Lazy       (Text, toStrict)
import           Discord              (DiscordHandler, restCall)
import qualified Discord.Requests     as R (ChannelRequest (..),
                                            ReactionTiming (..))
import           Discord.Types        (Message, ReactionInfo, reactionChannelId,
                                       reactionMessageId)
import           Network.Discord      (DiscordRequest (..), Reply)

-- command function/type = DiscordHandler constructor
-- command :: Text -> message -> DH()
-- conditional execution of DH or

type CommandData = (Message, Text)
type ReactionData = ReactionInfo

type Command = StateT CommandData DiscordHandler
type Reaction = ReaderT ReactionData DiscordHandler

instance DiscordRequest Command where
    dis = lift

instance DiscordRequest Reaction where
    dis = lift

instance Exposes Message Command where
    askExposed = gets fst

instance Exposes Text Command where
    askExposed = gets snd

instance Exposes Message Reaction where
    askExposed = do
        ch <- asks reactionChannelId
        id <- asks reactionMessageId
        Right m <- dis . restCall $ R.GetChannelMessage (ch, id)
        return m

instance Reply Command
instance Reply Reaction

data Action r a where
    MkCommand :: Text -> Command () -> CommandAction a -> CommandAction a
    MkReaction :: Text -> Reaction () -> ReactionAction a -> ReactionAction a
    Done :: a -> Action r a

type CommandAction = Action CommandData
type ReactionAction = Action ReactionData

instance Functor (Action t)

instance Applicative (Action t) where
    pure = Done
    f <*> Done a           = fmap (\f' -> f' a) f
    f <*> MkCommand s d a  = MkCommand s d (f <*> a)
    f <*> MkReaction s d a = MkReaction s d (f <*> a)

instance Monad (Action t) where
    return = pure
    Done a >>= f           = f a
    MkCommand s d a >>= f  = MkCommand s d $ a >>= f
    MkReaction s d a >>= f = MkReaction s d $ a >>= f

command :: Text -> Command () -> CommandAction ()
command a d = MkCommand a d (Done ())

reaction :: Text -> Reaction () -> ReactionAction ()
reaction a d = MkReaction a d (Done ())

-- interpret takes
-- an action identifier :: Text (Command alias, Emoji)
-- an action data :: r (MessageData, ReactionData)
-- a matching action :: Action r a (Command, Reaction)
-- and returns a valid DiscordHandler

type Identifier = Text
interpret :: Identifier -> r -> Action r a -> DiscordHandler ()
interpret _ _ (Done _)             = pure ()
interpret s r (MkCommand s' dh next) = if s == s' then evalStateT dh r else interpret s r next
interpret s r (MkReaction s' dh next) =
    if s /= s'
    then interpret s r next
    else runReaderT dh' r
    where dh' :: Reaction ()
          dh' = do
            let st = toStrict s
            ch <- asks reactionChannelId
            id <- asks reactionMessageId
            Right r <- dis $ restCall $ R.GetReactions (ch, id) st (2, R.LatestReaction)
            guard $ length r <= 1
            dis $ restCall $ R.CreateReaction (ch, id) st
            dh

