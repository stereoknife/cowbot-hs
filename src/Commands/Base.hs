{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Commands.Base (runCommand, Command, parse, parse') where

import           Control.Applicative  (Alternative)
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT,
                                       runReaderT)
import           Control.Monad.State  (MonadState (state), StateT, evalStateT)

import           Control.Monad.Trans  (MonadTrans (..))
import           Data.Text            (Text)
import           Discord              (DiscordHandler)
import           Discord.Types        (Message)
import           Parser               (Parser (runParser))

--type CommandData b a = ReaderT Message (StateT Text DiscordHandler) a
-- type Command = CommandData () ()

newtype CommandT m a = CommandT { runCommandM :: ReaderT Message (StateT Text m) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadFail, MonadIO, MonadReader Message, MonadState Text)

instance MonadTrans CommandT where
  lift = CommandT . lift . lift

type Command a = CommandT DiscordHandler a

runCommand :: Command a -> Message -> Text -> DiscordHandler a
runCommand c m t = evalStateT (runReaderT (runCommandM c) m) t

parse :: (MonadState Text m, Semigroup a) => Parser a -> m (Maybe a)
parse f = state $ \t -> do
  p <- return $ runParser f t
  case p of Just (v, rest) -> (Just v, rest)
            _              -> (Nothing, "")


parse' :: (MonadState Text m, Monoid a) => Parser a -> m a
parse' f = do
  p <- parse f
  return $ case p of Just v -> v
                     _      -> mempty


permission :: Message -> Bool
permission = undefined
