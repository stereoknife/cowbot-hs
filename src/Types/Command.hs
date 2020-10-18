{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}

module Types.Command where

import           Control.Monad.Reader (MonadReader, asks)
import           Data.Function
import           Data.Text            (Text)
import           Discord              (restCall)
import qualified Discord.Requests     as R
import qualified Discord.Types        as D
import           Parser.Parser        (alias)
import           Types.Discord        (DiscordHandler, liftDH)
import           Types.Parser         (Par, par)

class Monad m => Permission m where
    check :: m Bool

class (Permission m, Par m, MonadReader D.Message m) => Command m where
    reply :: Text -> m ()
    whisper :: Text -> m ()
    react :: Text -> m ()
    embed :: D.CreateEmbed -> m ()

instance Permission DiscordHandler where
    check = return True

instance Command DiscordHandler where
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
