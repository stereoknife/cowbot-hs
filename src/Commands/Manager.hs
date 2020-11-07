{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}

module Commands.Manager where

import           Commands.Bless      (bless)
import           Commands.Clap       (clap)
import           Commands.Translate  (comTranslate)
import           Commands.Youtube    (yt)
import           Control.Applicative (Alternative (empty))
import           Parser.Parser       (alias, prefix)
import           Types               (MessageData, Parser (..), Reply,
                                      Translate)
import           Types.Discord       (Command, MonadIO, liftIO)

commandSwitch :: Command ()
commandSwitch = do
    parse prefix >>= extract
    a <- parse alias  >>= extract
    if
        | a == "clap"  -> clap
        | a == "bless" -> bless
        | a == "t"     -> comTranslate
        | a == "yt"    -> yt
        | otherwise    -> return ()

    where extract (Just x) = pure x
          extract Nothing  = empty
