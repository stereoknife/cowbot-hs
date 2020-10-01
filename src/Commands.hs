{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands (commandSwitch, Command(..), runCommand) where

import           Commands.Base        (Command, parse, runCommand)
import           Commands.Bless       (bless)
import           Commands.Clap        (clap)
import           Commands.Translate   (comTranslate)
import           Commands.Youtube     (yt)
import           Parser               (alias)

import           Control.Monad.Reader (asks)
import           Control.Monad.Trans  (MonadTrans (lift))
import           Debug.Trace          (trace)
import           Discord              (restCall)
import qualified Discord.Requests     as R
import           Discord.Types        (Message (messageChannel))


commandSwitch :: Command
commandSwitch = do
  return $ trace "command found"
  al <- parse alias
  ch <- asks messageChannel
  let is b = case al of Just a -> a == b
                        _      -> False
  if
    | is "clap" -> clap
    | is "bless" -> bless
    | is "yt" -> yt
    | is "t" -> comTranslate
    | otherwise -> return () -- lift $ lift $ (restCall $ R.CreateMessage ch "bad command") >> pure ()
  pure ()

