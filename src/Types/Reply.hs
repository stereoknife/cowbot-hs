{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Types.Reply where

import           Control.Monad.Reader (asks)
import           Data.Text            (Text, pack)
import           Discord              (restCall)
import qualified Discord.Requests     as R
import qualified Discord.Types        as D
import           Types.Discord        (DiscordHandler, liftDH)

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

instance Reply DiscordHandler where
    reply t = do
        ch <- asks D.messageChannel
        liftDH $ restCall $ R.CreateMessage ch t
        return ()

    whisper t = do
        au <- asks $ D.userId . D.messageAuthor
        Right ch <- liftDH $ restCall $ R.CreateDM au
        liftDH $ restCall $ R.CreateMessage (D.channelId ch) t
        return ()

    react e = do
        c <- asks D.messageChannel
        m <- asks D.messageId
        liftDH $ restCall $ R.CreateReaction (c, m) e
        return ()

    embed e = do
        ch <- asks D.messageChannel
        liftDH $ restCall $ R.CreateMessageEmbed ch "" e
        return ()
