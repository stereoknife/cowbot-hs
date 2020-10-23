{-# LANGUAGE MultiWayIf #-}

module Commands.Manager where

import           Commands.Bless      (bless)
import           Commands.Clap       (clap)
import           Commands.Translate  (comTranslate)
import           Commands.Youtube    (yt)
import           Control.Applicative (Alternative (empty))
import           Parser.Parser       (alias, prefix)
import           Types               (MessageReader, Par (..), Reply, Translate)
import           Types.Discord       (MonadIO)

commandSwitch :: (Reply m, MessageReader m, Par m, MonadFail m, Alternative m, MonadIO m, Translate m) => m ()
commandSwitch = do
    par prefix >>= extract
    a <- par alias  >>= extract
    if
        | a == "clap"  -> clap
        | a == "bless" -> bless
        | a == "t"     -> comTranslate
        | a == "yt"    -> yt
        | otherwise    -> return ()

    where extract (Just x) = pure x
          extract Nothing  = empty
