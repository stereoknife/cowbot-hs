{-# LANGUAGE FlexibleContexts #-}
module Commands.Help where

import           Parser.Parser (flag)
import           Types         (Message, Parser, Reply (reply), parse)

help :: (Reply m, Parser m) => m ()
help = do
  vm <- parse $ flag "version"
  let v = case vm of Nothing -> False
                     Just _  -> True
  reply
    "__**Cowbot command reference**__\n\
    \**clap:** echoes a message with claps\n\
    \**bless:** blesses the chat with a random passage from the bible\n\
    \**yt:** searches youtube and posts the first result\n\
    \**t:** translates some text into english\
    \"

  return ()

