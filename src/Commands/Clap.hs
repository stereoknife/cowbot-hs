{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Clap where

import           Bot.Internal  (Parser, Reply, parse, reply)
import           Data.Maybe    (fromMaybe)
import           Data.Text     (intercalate)
import           Parser.Parser (arg, args, flag)

clap :: (Reply m, Parser m) => m ()
clap = do
  --e <- parse $ (flag "e") >> arg
  ar <- parse args
  let inr = fromMaybe "👏" Nothing
  reply $ intercalate inr (fromMaybe [""] ar) <> inr

