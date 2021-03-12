{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}

module Commands.Manager where


{-}
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
-}
