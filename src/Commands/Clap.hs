{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands.Clap (clap) where

import           Control.Applicative.Combinators (many)
--import           Data.Command                    (Command (Command))
import           Data.Maybe                      (fromMaybe)
import           Data.Parse                      (Parse (..))
import           Data.Text.Lazy                  (intercalate)
import           Data.Text.Lazy.Builder          (toLazyText)
import           Network.Discord                 (Reply, reply)
import           Howdy.Parser             (word)
import           UnliftIO                        (MonadIO (liftIO))

-- temp suppress errors
-- clap = undefined
-- --------------------

clap :: (MonadIO m, Reply m, Parse m) => m ()
clap = do
  --e <- parse $ (flag "e") >> arg
  ar <- parse $ many word
  liftIO $ print ar
  let inr = fromMaybe "👏" Nothing
  reply $ intercalate inr (fromMaybe [""] ar) <> inr

