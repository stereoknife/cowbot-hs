{-# LANGUAGE OverloadedStrings #-}

module Commands.Clap where

import           Commands.Base             (Command, parse)
import           Control.Monad.Combinators (empty)
import           Control.Monad.Reader      (asks, lift)
import qualified Data.Text                 as T
import           Discord                   (restCall)
import qualified Discord.Requests          as R
import           Discord.Types             (Message (messageChannel))
import           Parser                    (args)


clap :: Command ()
clap = do
  ar <- parse args
  ch <- asks messageChannel
  cl <- return $ ar >>= (return . T.intercalate "👏") >>= (return . (<> "👏"))
  case cl of Just t  -> lift $ restCall $ R.CreateMessage ch t
             Nothing -> empty
  pure ()
