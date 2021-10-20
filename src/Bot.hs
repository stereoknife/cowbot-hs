{-# LANGUAGE MultiParamTypeClasses #-}
module Bot where
import Data.Text (Text)
import Discord (DiscordHandler)
import Discord.Types (Message (messageText))
import qualified Data.Text as T
import Control.Monad (guard)
import Control.Monad.Trans.Reader ( asks )
import Howdy.Bot (run, command, prefix)
import Howdy.Parser (word)
import Howdy.Actions (ActionContext, Reply (reply))
import Howdy.Context (Context(fctx))

-- howdy monad is parser + discordhandler

{-
run :: IO ()
run = bot $ -- bot is function that runs discord with preconfigured settings
  command -- commands are specified from parsed messages
  reaction -- reactions are specified from messages reacted to
-}

-- Option 1


bot :: a
bot = undefined 

cowbot :: Monad m => m ()
cowbot = bot $ do
  prefix ["boy howdy", "cowboy"]

  command
    "bless"
    "blesses the chat"
    bless

  command
    "clap"
    "replaces spaces with claps"
    clap


bless :: ActionContext m => m ()
bless = do
  -- fetch bless data from api
  reply "here is the bless"

clap :: ActionContext m => m ()
clap = do
  a <- fctx argString
  reply . T.intercalate "clap" . T.words $ a

-- command end: needs these
-- how to handle aliases?

argString :: Message -> Text
argString = undefined 
