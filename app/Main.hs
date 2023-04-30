{-# LANGUAGE RebindableSyntax #-}

module Main where

import           Commands.AlBless             (alBless)
import           Commands.Bless               (bless)
import           Commands.Translate           (transCmd, transRec,
                                               transRecRandom)
--import           Commands.Units               (units)
import           Commands.Youtube             (yt)
import           Control.Monad.Reader         (MonadIO (liftIO),
                                               MonadReader (ask), asks)
import qualified Data.Text                    as T
import           Discord                      (def)
import           Howdy.Comptime.Bot           (command, prefixes, reaction)
import           Howdy.Comptime.Command       (CommandInput (args), alias, desc,
                                               run)
import qualified Howdy.Comptime.Reaction      as R
import           Howdy.Internal.Bot.Lifecycle (bot)
--import           Prelude                      hiding ((>>), (>>=))
import           GHC.Records
import           Howdy                        as H
import           Prelude                      hiding ((>>), (>>=))
import           System.Process               (readProcess)

import Howdy.Discord ( send, code )


main = bot $ do
    prefixes ["boy howdy", "🤠"]

    command "ping" $ do
        desc "just a ping"
        run (\_ -> send "pong")

    command "echo" $ do
        desc "just an echo"
        run (send . args)

    command "elongate" $ do
        desc "elongate"
        run (send . code . T.intersperse ' ' . args)


    command "clap" $ do
        desc "clap back"
        run (\t -> send $ T.intercalate "👏" (T.words t.args) <> "👏")


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


    -- command "xi" $ do
    --     desc "post xi"
    --     run $ const $ send "https://cdn.discordapp.com/attachments/481781014343974912/916265077471084614/dl.mp4"

    -- command "bashar" $ do
    --     desc "post bashar"
    --     run $ const $ send "https://media.discordapp.net/attachments/140942235670675456/914192060037345330/undefeated.png"

    -- command "uwu" $ do
    --     desc "uwuify text"
    --     run $ do
    --         ms <- asks (.args)
    --         uwu <- liftIO $ readProcess "uwuify" [] (T.unpack ms)
    --         send $ T.pack uwu
    --     -- disabled

    reaction (R.Unicode "🔣") $ do
        R.desc "no desc"
        R.run transRec

    reaction (R.Unicode "🗺️") $ do
        R.desc "no desc"
        R.run transRecRandom

    -- reaction (Unicode "♋")$ do
    --     R.desc "uwuifies text"
    --     R.run $ do
    --           ms <- asks (.args)
    --           uwu <- liftIO $ readProcess "uwuify" [] (T.unpack ms)
    --           send $ T.pack uwu
