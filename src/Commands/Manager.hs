{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}

module Commands.Manager where

import           Bot.Internal        (Command, MessageData, Parser (..), Reply)
import           Commands.Bless      (bless)
import           Commands.Clap       (clap)
import           Commands.Translate  (translate)
import           Commands.Youtube    (yt)
import           Control.Applicative (Alternative (empty))
import           Parser.Parser       (alias, prefix)

commandSwitch :: Command ()
commandSwitch = do
    parse prefix >>= extract
    a <- parse alias  >>= extract
    if
        | a == "clap"  -> clap
        | a == "bless" -> bless
        | a == "t"     -> translate
        | a == "yt"    -> yt
        | otherwise    -> return ()

    where extract (Just x) = pure x
          extract Nothing  = empty
