{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.Discord where

import           Data.Discord     (Exposes (asksExposed))
import           Data.Text.Lazy   (Text, toStrict)
import           Discord          (DiscordHandler, restCall)
import qualified Discord.Requests as R
import           Discord.Types    (Channel (channelId), CreateEmbed,
                                   Message (messageAuthor, messageChannel, messageId),
                                   User (userId))

class Monad m => DiscordRequest m where
    dis :: DiscordHandler a -> m a

class (Monad m, DiscordRequest m, Exposes Message m) => Reply m where
    reply :: Text -> m ()
    whisper :: Text -> m ()
    react :: Text -> m ()
    embed :: CreateEmbed -> m ()

    reply t = do
        ch <- asksExposed messageChannel
        dis $ restCall $ R.CreateMessage ch $ toStrict t
        pure ()

    whisper t = do
        au <- asksExposed $ userId . messageAuthor
        ch <- dis $ restCall $ R.CreateDM au
        case ch of Right ch' -> dis (restCall $ R.CreateMessage (channelId ch') $ toStrict t) >> pure ()
                   Left _ -> pure ()

    react e = do
        c <- asksExposed messageChannel
        m <- asksExposed messageId
        dis $ restCall $ R.CreateReaction (c, m) $ toStrict e
        pure ()

    embed e = do
        ch <- asksExposed messageChannel
        dis $ restCall $ R.CreateMessageEmbed ch "" e
        pure ()
