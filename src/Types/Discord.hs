{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Discord ( MonadIO (..)
                     , MessageReader (..)
                     , DiscordHandler (..)
                     , interpret
                     , liftDH
                     ) where

import           Control.Applicative  (Alternative)
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT, lift,
                                       runReaderT)
import           Control.Monad.State  (MonadState, StateT (StateT), evalStateT)
import           Data.Text            (Text)
import qualified Discord              as D
import           Discord.Types        (Message (messageText))
import qualified Discord.Types        as D


newtype DiscordHandler a = DiscordHandler { extractHandler :: StateT Text (ReaderT D.Message D.DiscordHandler) a }
    deriving (Functor, Applicative, Monad, MonadFail, MonadReader D.Message, MonadIO, Alternative, MonadState Text)

type MessageReader = MonadReader D.Message

interpret :: D.Message -> DiscordHandler a -> D.DiscordHandler a
interpret m d = let dh = extractHandler d
                    st = evalStateT dh $ messageText m
                    rd = runReaderT st m
                 in rd

liftDH :: D.DiscordHandler a -> DiscordHandler a
liftDH = DiscordHandler . lift . lift
