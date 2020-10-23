module Types.Command where

import           Data.Text     (Text)
import           Types.Discord (DiscordHandler (..))

newtype CommandId = CommandId Text

instance Show CommandId where
    show (CommandId id) = show id

data Command m = Command { exec      :: m ()
                         , commandId :: CommandId
                         , alias     :: [Text]
                         , desc      :: Text
                         }


command :: [Text] -> m () -> Command m
command a e = Command { commandId = CommandId $ head a
                      , alias = a
                      , exec = e
                      , desc = ""
                      }

describe :: Text -> Command m -> Command m
describe d c = c{desc = d}


class Monad m => Permission m where
    check :: m Bool

instance Permission DiscordHandler where
    check = return True
