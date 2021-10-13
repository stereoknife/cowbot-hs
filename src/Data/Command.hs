{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Data.Command where

import           Data.Kind    (Type)
import           Data.Proxy   (Proxy)
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

type Command :: Symbol -> (Type -> Type) -> Type -> Type
newtype Command n m a = Command { runCommand :: m a }

type Reaction :: Symbol -> (Type -> Type) -> Type -> Type
newtype Reaction e m a = Reaction { runReaction :: m a }

class CLI w where
    run :: w f a -> f a

instance CLI (Command n) where
    run = runCommand

instance CLI (Reaction e) where
    run = runReaction

-- get command name from type
class Named t where
    name :: t -> String

instance KnownSymbol n => Named (Command n m a) where
    name _ = symbolVal (undefined :: Proxy n)

-- equal to class it seems
name' :: forall n m a. KnownSymbol n => Command n m a -> String
name' _ = symbolVal (undefined :: Proxy n)
