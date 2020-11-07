{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Types.Discord ( MonadIO (..)
                     , DiscordFTL (..)
                     , DiscordRequest (..)
                     , Command
                     , Reaction
                     , interpret
                     ) where

import           Control.Applicative
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT (..), ask,
                                       asks, lift, runReaderT, withReaderT)
import           Data.Text            (Text)

import           Control.Monad.State
import           Discord              (DiscordHandler)
import           Discord.Types        (Message, ReactionInfo)


class (Monad m) => DiscordRequest m where
    dis :: DiscordHandler a -> m a

instance DiscordRequest (DiscordFTL r) where
    dis = DiscordFTL . lift . lift

newtype DiscordFTL r a = DiscordFTL { runDiscordFTL :: StateT Text (ReaderT r DiscordHandler) a  }
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, Alternative, MonadReader r, MonadState Text)

type Command = DiscordFTL Message
type Reaction = DiscordFTL ReactionInfo

interpret' :: r -> (r -> Text) -> DiscordFTL r a -> DiscordHandler a
interpret' msg f = runReaderT' msg . evalStateT' (f msg) . runDiscordFTL
    where runReaderT' = flip runReaderT
          evalStateT' = flip evalStateT

class DiscordAction r d where
    interpret :: r -> (r -> Text) -> d a -> DiscordHandler a

instance DiscordAction Message Command where
    interpret msg f = runReaderT' msg . evalStateT' (f msg) . runDiscordFTL
        where runReaderT' = flip runReaderT
              evalStateT' = flip evalStateT

instance DiscordAction ReactionInfo Reaction where
    interpret ri f = runReaderT' ri . evalStateT' (f ri) . runDiscordFTL
        where runReaderT' = flip runReaderT
              evalStateT' = flip evalStateT
