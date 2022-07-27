module Main where

import           Commands.Bless         (bless)
import           Commands.AlBless         (alBless)
import           System.Process         (readProcess)
import qualified Data.Text as T
import Howdy.Bot ( bot, command, prefixes )
import Howdy.Command ( alias, desc, run, CommandInput(channel, args) )
import qualified Howdy.Internal.Command as C
import Howdy.Internal.Discord ( send )
import Control.Monad.Reader ( MonadIO(liftIO), MonadReader(ask), asks )
import Commands.Youtube (yt)
import Howdy.Internal.Reaction (EmojiIdentifier (..))
import qualified Howdy.Internal.Reaction as R
import Commands.Translate (transCmd)

main :: IO ()
main = bot $ do
    prefixes ["boy howdy", "🤠"]

    command "ping" $ do
        desc "just a ping"
        run $ send "pong"
        -- disabled

    command "echo" $ do
        desc "just an echo"
        run $ do
            t <- ask
            send t.args

    command "elongate" $ do
        desc "elongate"
        run $ do
            t <- ask
            send $ "`" <> T.intersperse ' ' t.args <> "`"

    command "clap" $ do
        desc "clap back"
        run $ do
            t <- ask
            send $ T.intercalate "👏" (T.words t.args) <> "👏"

    command "bless" $ do
        desc "bless the chat"
        run bless

    command "al-bless" $ do
        desc "al-bless the chat"
        run alBless

    command "t" $ do
        alias ["translate"]
        desc "translate something to english"
        run transCmd

    command "yt" $ do
        alias ["youtube"]
        desc "search a video on youtube and post the first result"
        run yt

    command "xi" $ do
        desc "post xi"
        run $ send "https://cdn.discordapp.com/attachments/481781014343974912/916265077471084614/dl.mp4"

    command "bashar" $ do
        desc "post bashar"
        run $ send "https://media.discordapp.net/attachments/140942235670675456/914192060037345330/undefeated.png"

    command "uwu" $ do
        desc "uwuify text"
        run $ do
            ms <- asks (.args)
            uwu <- liftIO $ readProcess "uwuify" [] (T.unpack ms)
            send $ T.pack uwu

    -- reaction (Unicode "🔣") $ do
    --     R.desc "no desc"
    --     R.run transRec

    -- reaction (Unicode "🗺️") $ do
    --     R.desc "no desc"
    --     R.run transRecRandom

    -- reaction (Unicode "♋")$ do
    --     R.desc "uwuifies text"
    --     R.run $ do
    --           ms <- asks (.args)
    --           uwu <- liftIO $ readProcess "uwuify" [] (T.unpack ms)
    --           send $ T.pack uwu
