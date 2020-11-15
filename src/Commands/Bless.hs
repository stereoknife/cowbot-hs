{-# LANGUAGE FlexibleContexts #-}

module Commands.Bless (bless) where

import           Bot.Internal (Net, Reply (reply))
import           Net.Bless    (blessRequest)

bless :: (Reply m, Net m) => m ()
bless = do
    res <- blessRequest
    case res of Right a -> reply a
                _       -> return ()
