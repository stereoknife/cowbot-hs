{-# LANGUAGE FlexibleContexts #-}
module Commands.Uwu where

import           Data.Discord        (Exposes, asksExposed)
import           Data.Parse          (Parse, parse)
import qualified Data.Text           as T (unpack)
import qualified Data.Text.Lazy      as L (pack, unpack)
import           Discord.Types       (Message, messageText)
import           Network.Discord     (Reply, reply)
import           Parser.Constructors (rest)
import           UnliftIO            (MonadIO, liftIO)
import           UnliftIO.Process    (readProcess)

uwu :: (MonadIO m, Reply m, Parse m, MonadFail m) => m ()
uwu = do
  Just ms <- parse rest
  uwu <- liftIO $ readProcess "uwuify" [] (L.unpack ms)
  reply $ L.pack uwu

uwuR :: (MonadIO m, Reply m, Exposes Message m, MonadFail m) => m ()
uwuR = do
  ms <- asksExposed messageText
  uwu <- liftIO $ readProcess "uwuify" [] (T.unpack ms)
  reply $ L.pack uwu
