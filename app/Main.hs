{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import           Commands.AlBless             (alBless)
import           Commands.Bless               (bless)
import           Commands.Translate           (transCmd, transRec,
                                               transRecRandom)
import           Commands.Youtube             (yt)
import           Control.Monad.Reader         (MonadIO (liftIO),
                                               MonadReader (ask), asks)
import qualified Data.Text                    as T
import           Discord                      (def)
import           Prelude                      hiding ((>>), (>>=))

import qualified Howdy as H ((>>))
import Howdy.Bot ( bot, prefixes, command, reaction, onInit )
import Howdy.Command (CommandInput (args), alias, desc,run)
import Howdy.Discord ( send, code )
import qualified Howdy.Reaction      as R

newtype BotStore = BotStore Bool

main = bot H.do
    prefixes ["boy howdy", "🤠"]

    command "ping" H.do
        desc "just a ping"
        run $ const $ send "pong"

    command "echo" H.do
        desc "just an echo"
        run $ send . args

    command "elongate" H.do
        desc "elongate"
        run $ send . code . T.intersperse ' ' . args


    command "clap" H.do
        desc "clap back"
        run $ \t -> send $ T.intercalate "👏" (T.words t.args) <> "👏"


    command "bless" H.do
        desc "bless the chat"
        run bless


    command "al-bless" H.do
        desc "al-bless the chat"
        run alBless


    command "t" H.do
        alias ["translate"]
        desc "translate something to english"
        run transCmd


    command "yt" H.do
        alias ["youtube"]
        desc "search a video on youtube and post the first result"
        run yt

    -- command "uwu" H.do
    --     desc "uwuify text"
    --     run $ \t -> do
    --         uwu <- liftIO $ readProcess "uwuify" [] (T.unpack t.args)
    --         send $ T.pack uwu

    reaction (R.Unicode "🔣") H.do
        R.desc "no desc"
        R.run transRec

    reaction (R.Unicode "🗺️") H.do
        R.desc "no desc"
        R.run transRecRandom

    -- reaction (Unicode "♋") H.do
    --     R.desc "uwuifies text"
    --     R.run $ \t -> do
    --         uwu <- liftIO $ readProcess "uwuify" [] (T.unpack t.args)
    --         send $ T.pack uwuu