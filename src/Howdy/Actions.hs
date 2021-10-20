{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Howdy.Actions where
import Data.Text (Text)
import Discord (DiscordHandler)
import Discord.Types (Emoji, User, Message, Channel, Guild)
import Howdy.Context (Contexts)

data Action where
    Command :: Text -> Text -> DiscordHandler () -> Action
    Reaction :: Emoji -> Text -> DiscordHandler () -> Action

type ActionContext m = (Contexts [Message, User, Channel, Guild] m, Reply m)

class Monad m => Reply m where
  reply :: Text -> m ()
