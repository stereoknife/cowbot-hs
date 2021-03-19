{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}



module Data.Discord (Exposes (..), ExposesMultiple, Updates (..)) where

import           Control.Monad.Reader (MonadReader (ask), ReaderT)
import           Control.Monad.State  (MonadState (get, put), StateT)
import           Data.Kind            (Constraint)

class Monad m => Exposes a m where
    askExposed :: m a
    asksExposed :: (a -> b) -> m b
    asksExposed f = f <$> askExposed

instance Monad m => Exposes r (ReaderT r m) where
    askExposed = ask

instance Monad m => Exposes r (StateT r m) where
    askExposed = get

type family ExposesMultiple k m :: Constraint where
    ExposesMultiple '[] _ = ()
    ExposesMultiple (k : ks) m = (Exposes k m, ExposesMultiple ks m)

class Exposes a m => Updates a m where
    setExposed :: a -> m ()

instance (Monad m) => Updates r (StateT r m) where
    setExposed = put
