{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Base (runCommand, Command, parse, parse', send) where

import           Control.Monad.Reader (ReaderT, asks, runReaderT)
import           Control.Monad.State  (MonadState (state), StateT, evalStateT)
import           Control.Monad.Trans  (MonadTrans (lift))
import           Data.Text            (Text)
import           Discord              (DiscordHandle, DiscordHandler)
import           Discord.Types        (Message)
import           Parser               (Parser (runParser))

type CommandData b a = ReaderT Message (StateT Text DiscordHandler) a
type Command = CommandData () ()

send :: ReaderT DiscordHandle IO a -> ReaderT Message (StateT Text (ReaderT DiscordHandle IO)) a
send = lift . lift

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


runCommand :: Monad m => ReaderT r (StateT s m) a -> r -> s -> m a
runCommand c m t = evalStateT s t
  where s = runReaderT c m


permission :: Message -> Bool
permission = undefined
