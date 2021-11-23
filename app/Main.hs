module Main where

import           Commands.Bless         (bless)
import           Commands.Translate     (transCmd, transRec, transRecRandom)
import           Commands.Youtube       (yt)
import           Control.Applicative    (Alternative (empty, many))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text              as T
import           Howdy.Action           (CommandRunner, alias, aliases, desc,
                                         emoji, run)
import           Howdy.Bot              (bot, command, prefixes, reaction)
import           Howdy.Context          (fctx)
import           Howdy.Discord.Class    (Message (messageText),
                                         MonadReply (reply))
import           Howdy.Parser           (MonadParse (parse), char, rest,
                                         whitespace, word)
import           System.Process         (readProcess)

main :: IO ()
main = bot $ do
    prefixes ["boy howdy", "🤠"]

    command $ do
        alias "ping"
        desc "just a ping"
        run $ reply "pong"
        -- disabled

    command $ do
        alias "echo"
        desc "just an echo"
        run $ parse rest >>= reply

    command $ do
        alias "elongate"
        desc "it elongates"
        run $ do
            t <- parse (many whitespace >> rest)
            reply $ "`" <> T.intersperse ' ' t <> "`"

    command $ do
        alias "clap"
        desc "claps back"
        run $ do
            t <- parse (many word)
            reply $ T.intercalate "👏" t <> "👏"

    command $ do
        alias "bless"
        desc "blesses the chat"
        run bless

    command $ do
        aliases ["t", "translate"]
        desc "translates something to english"
        run transCmd

    command $ do
        aliases ["yt", "youtube"]
        desc "searches a video on youtube and posts the first result"
        run yt

    command $ do
        alias "uwu"
        desc "uwuifies text"
        run $ do
            ms <- parse rest
            uwu <- liftIO $ readProcess "uwuify" [] (T.unpack ms)
            reply $ T.pack uwu

    reaction $ do
        emoji "🔣"
        desc "no desc"
        run transRec

    reaction $ do
        emoji "🗺️"
        desc "no desc"
        run transRecRandom

    reaction $ do
        emoji "♋"
        desc "uwuifies text"
        run $ do
            ms <- fctx messageText
            uwu <- liftIO $ readProcess "uwuify" [] (T.unpack ms)
            reply $ T.pack uwu
