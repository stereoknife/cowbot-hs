module Secrets ( token
               , yt_key
               , tr_key
               ) where

import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           GHC.Base           (Alternative ((<|>)))
import           System.Environment

token :: IO T.Text
token = T.pack <$> getEnv "token" <|> TIO.readFile "./token.secret"

yt_key :: IO T.Text
yt_key = T.pack <$> getEnv "g_key" <|> TIO.readFile "./google.secret"

tr_key :: IO T.Text
tr_key = T.pack <$> getEnv "g_key" <|> TIO.readFile "./google.secret"
