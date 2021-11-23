module Main where

import           Commands.Bless      (bless)
import           Commands.Translate  (transCmd, transRec, transRecRandom)
import           Control.Applicative (Alternative (empty, many))
import qualified Data.Text           as T
import           Howdy.Action        (CommandRunner, alias, aliases, desc,
                                      emoji, run)
import           Howdy.Bot           (bot, command, prefixes, reaction)
import           Howdy.Discord.Class (MonadReply (reply))
import           Howdy.Parser        (MonadParse (parse), char, rest,
                                      whitespace, word)

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
        run empty

    command $ do
        alias "uwu"
        desc "uwuifies text"
        run empty

    reaction $ do
        emoji "🔣"
        desc "no desc"
        run transRec

    reaction $ do
        emoji "🗺️"
        desc "no desc"
        run transRecRandom
