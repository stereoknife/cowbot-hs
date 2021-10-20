{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Howdy.Context where

import Data.Kind ( Type, Constraint )
import GHC.TypeLits ( Symbol )
import Discord.Types ( Message(messageAuthor), User )

class Monad m => Context a m where
    ctx :: m a
    fctx :: (a -> b) -> m b
    fctx f = fmap f ctx

type family Contexts k m :: Constraint where
    Contexts '[] _ = ()
    Contexts (k : ks) m = (Context k m, Contexts ks m)

class Functor f => Property (a :: Symbol) f where
    type Key a :: Type
    prop :: f (Key a)

instance (Context Message m, Functor m) => Property "author" m where
    type Key "author" = User
    prop = fctx @Message messageAuthor

class Exposes a b where
    exp :: a -> b