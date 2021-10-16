module Bot where
import Data.Text (Text)
import Discord (DiscordHandler)
import Discord.Types (Message (messageText))
import qualified Data.Text as T
import Control.Monad (guard)
import Howdy.Actions ( Command, Reply(reply), command )
import Control.Monad.Trans.Reader ( asks )

-- howdy monad is parser + discordhandler

{-
run :: IO ()
run = bot $ -- bot is function that runs discord with preconfigured settings
  command -- commands are specified from parsed messages
  reaction -- reactions are specified from messages reacted to
-}

echo :: Command ()
echo = command
     $ asks messageText >>= reply