module Commands.Uwu where

import           Data.Parse          (Parse, parse)
import           Data.Text.Lazy      (pack, unpack)
import           Network.Discord     (Reply, reply)
import           Parser.Constructors (rest)
import           UnliftIO            (MonadIO, liftIO)
import           UnliftIO.Process    (readProcess)

uwu :: (MonadIO m, Reply m, Parse m, MonadFail m) => m ()
uwu = do
  Just ms <- parse rest
  uwu <- liftIO $ readProcess "uwuify" [] (unpack ms)
  reply $ pack uwu
