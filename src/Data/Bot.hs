{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Data.Bot where

import           Control.Monad.State (StateT, evalStateT, gets, lift)
import           Data.Discord        (Exposes (askExposed))
import           Data.Text.Lazy      (Text)
import           Discord             (DiscordHandler)
import           Discord.Types       (Message)
import           Network.Discord     (DiscordRequest (..), Reply)

-- command function/type = DiscordHandler constructor
-- command :: alias -> message -> DH()
-- conditional execution of DH or

type Alias = Text

type DiscordContext r = StateT r DiscordHandler
type Command = DiscordContext (Message, Text)

instance DiscordRequest Command where
    dis = lift

instance Exposes Message Command where
    askExposed = gets fst
instance Exposes Text Command where
    askExposed = gets snd

instance Reply Command

data Action r a = Command Alias (DiscordContext r ()) (Action r a)
                | Done a
    deriving (Functor)

instance Applicative (Action t) where
    pure = Done
    f <*> Done a        = fmap (\f' -> f' a) f
    f <*> Command s d a = Command s d (f <*> a)

instance Monad (Action t) where
    return = pure
    Done a >>= f        = f a
    Command s d a >>= f = Command s d $ a >>= f

command :: Alias -> DiscordContext r () -> Action r ()
command a d = Command a d (Done ())

interpret :: Alias -> r -> Action r a -> DiscordHandler ()
interpret _ _ (Done _)             = pure ()
interpret s r (Command s' dh next) = if s == s' then evalStateT dh r else interpret s r next
