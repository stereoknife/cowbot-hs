{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}


module Data.Discord (Exposes (..), ExposesMultiple, Updates (..)) where

import           Data.Kind (Constraint)

class Monad m => Exposes a m where
    askExposed :: m a
    asksExposed :: (a -> b) -> m b
    asksExposed f = f <$> askExposed

class Exposes a m => Updates a m where
    setExposed :: a -> m ()

type family ExposesMultiple k m :: Constraint where
    ExposesMultiple '[] _ = ()
    ExposesMultiple (k : ks) m = (Exposes k m, ExposesMultiple ks m)


