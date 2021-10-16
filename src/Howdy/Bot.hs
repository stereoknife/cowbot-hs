module Howdy.Bot where

import Discord (runDiscord, def, RunDiscordOpts (..), DiscordHandler)
import UnliftIO.Concurrent (forkIO, readChan, Chan, newChan)
import Control.Monad (forever)
import qualified Data.Text.IO as TIO
import Discord.Types (Event (..))

newtype Bot = MkBot { run :: IO () }

bot :: r -> Bot
bot = undefined

botSkell :: IO ()
botSkell = do
    tok <- undefined
    outChan <- newChan :: IO (Chan String)

    -- threadId <- forkIO $ forever $ readChan outChan >>= putStrLn

    err <- runDiscord $ def { discordToken = tok
                            , discordOnEvent = undefined 
                            }

    TIO.putStrLn err
    
eventHandler :: Event -> DiscordHandler ()
eventHandler (MessageCreate m) = undefined
eventHandler _ = pure ()