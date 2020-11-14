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

module Bot.Internal.Discord ( MonadIO (..)
                            , DiscordFTL (..)
                            , DiscordRequest (..)
                            , Command
                            , Reaction
                            , interpret
                            ) where

import           Control.Applicative
import           Control.Monad.Reader (MonadReader, ReaderT (..), runReaderT)
import           Data.Text            (Text)

import           Control.Monad.Catch  (MonadThrow)
import           Control.Monad.State  (MonadIO (..), MonadState,
                                       MonadTrans (lift), StateT (StateT),
                                       evalStateT)
import           Discord              (DiscordHandler)
import           Discord.Types        (Message, ReactionInfo)

newtype DiscordFTL r a = DiscordFTL { runDiscordFTL :: StateT Text (ReaderT r DiscordHandler) a  }
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, Alternative, MonadReader r, MonadState Text, MonadThrow)

type Command = DiscordFTL Message
type Reaction = DiscordFTL ReactionInfo


class (Monad m) => DiscordRequest m where
    dis :: DiscordHandler a -> m a

instance DiscordRequest (DiscordFTL r) where
    dis = DiscordFTL . lift . lift


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
