{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Data where

import           Control.Applicative           (Alternative (empty))
import           Control.Monad.Reader          (ask, asks)
import           Discord                       (restCall)
import qualified Discord.Internal.Rest.Channel as R
import           Discord.Types                 (Message,
                                                ReactionInfo (reactionChannelId, reactionMessageId))
import           Types.Discord                 (DiscordFTL,
                                                DiscordRequest (dis))

class Monad m => MessageData m where
    message :: m Message
    askMessage :: (Message -> a) -> m a

class Monad m => ReactionData m where
    reaction :: m ReactionInfo
    askReaction :: (ReactionInfo -> a) -> m a

instance MessageData (DiscordFTL Message) where
    message = ask
    askMessage = asks

instance MessageData (DiscordFTL ReactionInfo) where
    message = do
        cid <- asks reactionChannelId
        mid <- asks reactionMessageId
        msg <- dis $ restCall $ R.GetChannelMessage (cid, mid)
        case msg of Left _  -> empty
                    Right a -> return a
    askMessage f = message >>= return . f

instance ReactionData (DiscordFTL ReactionInfo) where
    reaction = ask
    askReaction = asks
