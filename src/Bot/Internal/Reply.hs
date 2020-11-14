{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Bot.Internal.Reply where

import           Bot.Internal.Discord (Command, Reaction, dis)
import           Control.Applicative  (Alternative (empty))
import           Control.Monad.Reader (asks)
import           Data.Text            (Text, pack)
import           Discord              (restCall)
import qualified Discord.Requests     as R
import qualified Discord.Types        as D

class (Monad m) => Reply m where
    reply :: Text -> m ()
    whisper :: Text -> m ()
    react :: Text -> m ()
    embed :: D.CreateEmbed -> m ()

class (Show s) => Textable s where
    tshow :: s -> Text
    tshow = pack . show

instance Textable String
instance Textable Int
instance Textable Integer
instance Textable Float
instance Textable Char

instance Textable Text where
    tshow = id

instance Reply Command where
    reply t = do
        ch <- asks D.messageChannel
        dis $ restCall $ R.CreateMessage ch t
        return ()

    whisper t = do
        au <- asks $ D.userId . D.messageAuthor
        ch <- dis $ restCall $ R.CreateDM au
        case ch of Left _ -> empty
                   Right ch' -> dis $ restCall $ R.CreateMessage (D.channelId ch') t
        return ()

    react e = do
        c <- asks D.messageChannel
        m <- asks D.messageId
        dis $ restCall $ R.CreateReaction (c, m) e
        return ()

    embed e = do
        ch <- asks D.messageChannel
        dis $ restCall $ R.CreateMessageEmbed ch "" e
        return ()

instance Reply Reaction where
    reply t = do
        ch <- asks D.reactionChannelId
        dis $ restCall $ R.CreateMessage ch t
        return ()

    whisper t = do
        au <- asks D.reactionUserId
        ch <- dis $ restCall $ R.CreateDM au
        case ch of Left _ -> empty
                   Right ch' -> dis $ restCall $ R.CreateMessage (D.channelId ch') t
        return ()

    react e = do
        c <- asks D.reactionChannelId
        m <- asks D.reactionMessageId
        dis $ restCall $ R.CreateReaction (c, m) e
        return ()

    embed e = do
        ch <- asks D.reactionChannelId
        dis $ restCall $ R.CreateMessageEmbed ch "" e
        return ()
