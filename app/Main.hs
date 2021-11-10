module Main where

import           Commands.Bless      (bless)
import           Control.Applicative (Alternative (empty, many))
import qualified Data.Text           as T
import           Howdy.Action        (CommandRunner, action, alias, aliases,
                                      desc)
import           Howdy.Bot           (bot, command, prefixes, run)
import           Howdy.Discord.Class (Reply (reply))
import           Howdy.Parser        (MonadParse (parse), char, rest, word)

main :: IO ()
main = run . bot $ do
    prefixes ["boy howdy", "🤠"]

    command $ do
        alias "ping"
        desc "just a ping"
        action $ reply "pong"
        -- disabled

    command $ do
        alias "echo"
        desc "no desc"
        action $ parse rest >>= reply

    command $ do
        alias "elongate"
        desc "it elongates"
        action $ do
            t <- parse (many (char ' ') >> rest)
            reply $ "`" <> T.intersperse ' ' t <> "`"

    command $ do
        alias "clap"
        desc "claps back"
        action $ do
            t <- parse (many word)
            reply $ T.intercalate "👏" t <> "👏"

    command $ do
        alias "bless"
        desc "blesses the chat"
        action bless

    command $ do
        aliases ["t", "translate"]
        desc "translates something to english"
        action empty

    command $ do
        alias "uwu"
        desc "uwu"
        action empty

    command $ do
        aliases ["yt", "youtube"]
        desc "uwu"
        action empty
