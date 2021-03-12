{-# LANGUAGE FlexibleContexts #-}

module Commands.Bless (bless) where


-- temp suppress errors
bless = undefined
-- --------------------

{-
bless :: (Reply m, Net m) => m ()
bless = do
    res <- blessRequest
    case res of Right a -> reply a
                _       -> return ()
-}
