module Main where

import           Control.Applicative (Alternative (empty, many))
import qualified Data.Text           as T
import           Howdy.Action        (action, alias, aliases, desc)
import           Howdy.Bot           (bot, command, prefixes, run)
import           Howdy.Discord.Class (Reply (reply))
import           Howdy.Parser        (MonadParse (parse), char, rest)

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
        action $ do
            t <- parse rest
            case t of Just t' -> reply t'
                      _       -> pure ()

    command $ do
        alias "elongate"
        desc "it elongates"
        action $ do
            t <- parse (many (char ' ') >> rest)
            case t of Just t' -> reply $ "`" <> T.intersperse ' ' t' <> "`"
                      _       -> pure ()

    command $ do
        alias "bless"
        desc "blesses the chat"
        action empty

    command $ do
        aliases ["t", "translate"]
        desc "translates something to english"
        action empty

    command $ do
        alias "uwu"
        desc "uwu"
        action empty
