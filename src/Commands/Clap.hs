{-# LANGUAGE OverloadedStrings #-}

module Commands.Clap where

import           Commands.Base             (Command, parse, send)
import           Control.Monad.Combinators (empty)
import           Control.Monad.Reader      (asks)
import           Control.Monad.Trans       (MonadTrans (lift))
import qualified Data.Text                 as T
import           Discord                   (restCall)
import qualified Discord.Requests          as R
import           Discord.Types             (Message (messageChannel))
import           Parser                    (args)


clap :: Command
clap = do
  return $ print "clappin"
  ar <- parse args
  ch <- asks messageChannel
  cl <- return $ ar >>= (return . T.intercalate "👏") >>= (return . (<> "👏"))
  case cl of Just t  -> send $ restCall $ R.CreateMessage ch t
             Nothing -> empty
  pure ()
