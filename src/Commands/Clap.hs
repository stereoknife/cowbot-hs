{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Clap where

import           Data.Maybe    (fromMaybe)
import           Data.Text     (intercalate)
import           Parser.Parser (arg, args, flag)
import           Types         (Parser, Reply, parse, reply)
import           Types.Discord (MonadIO (liftIO))

clap :: (Reply m, Parser m, MonadIO m) => m ()
clap = do
  --e <- parse $ (flag "e") >> arg
  ar <- parse args
  let inr = fromMaybe "👏" Nothing
  reply $ intercalate inr (fromMaybe [""] ar) <> inr

