{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Howdy.Actions where
import Data.Text (Text)
import Discord (DiscordHandler)
import Discord.Types (Emoji, User, Message, Channel, Guild)
import Howdy.Context (Contexts)
import Data.Kind (Constraint, Type)
import Howdy.Parser
import Data.Maybe (fromMaybe)
import Howdy.Parser.Trans

data Command'
data Reaction'

type Command = Action Command'
type Reaction = Action Reaction'

data Action a where
    Command :: Text -> Text -> DiscordHandler () -> Action Command'
    Reaction :: Emoji -> Text -> DiscordHandler () -> Action Reaction'

type ActionContext m = (Contexts [Message, User, Channel, Guild] m, Reply m)

class Monad m => Reply m where
  reply :: Text -> m ()

newtype DiscordAction a = DiscordAction { runAction :: ParserT DiscordHandler a }
  deriving (Functor, Applicative, Monad, Alternative)

