module Commands.Help where

import           Control.Monad.Reader (asks, lift)
import           Discord              (restCall)
import qualified Discord.Requests     as R
import           Discord.Types        (Message (messageChannel))
import           Parser.Parser        (flag)
import           Types

help :: (Reply m, Par m) => m ()
help = do
  vm <- par $ flag "version"
  let v = case vm of Nothing -> False
                     Just _  -> True
  reply
    "__**Cowbot command reference**__\n\
    \**clap:** echoes a message with claps\n\
    \**bless:** blesses the chat with a random passage from the bible\n\
    \**yt:** searches youtube and posts the first result\n\
    \**t:** translates some text into english\
    \"

  return ()

