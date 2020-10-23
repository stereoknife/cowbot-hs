{-# LANGUAGE OverloadedStrings #-}

module Commands.Clap where

import           Control.Applicative
import           Data.Maybe          (fromMaybe)
import           Data.Text           (intercalate)
import           Parser.Parser       (arg, args, flag)
import           Types               (MessageReader, Par (..), Reply, par,
                                      reply)
import           UnliftIO            (MonadIO (liftIO))

clap :: (Reply m, MessageReader m, Par m, MonadIO m) => m ()
clap = do
  e <- par $ (flag "e") >> arg
  ar <- par args
  let inr = fromMaybe "👏" e
  reply $ intercalate inr (fromMaybe [""] ar) <> inr

