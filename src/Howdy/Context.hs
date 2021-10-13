{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}


module Howdy.Context where

import GHC.TypeLits (Symbol)
import Data.Kind (Type, Constraint)
import Discord.Types (Message (messageAuthor), User)

type Author = "author"

class Monad m => Exposes a m where
    askExposed :: m a
    asksExposed :: (a -> b) -> m b
    asksExposed f = f <$> askExposed

type family ExposesMultiple k m :: Constraint where
    ExposesMultiple '[] _ = ()
    ExposesMultiple (k : ks) m = (Exposes k m, ExposesMultiple ks m)

class Functor f => Context (a :: Symbol) f where
    type Key a :: Type
    ctx :: f (Key a)

instance (Exposes Message m, Functor m) => Context "author" m where
    type Key "author" = User
    ctx = asksExposed @Message messageAuthor


