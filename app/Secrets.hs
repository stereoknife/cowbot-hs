module Secrets ( discordToken
               , youtubeKey
               , translateKey
               ) where

import           Control.Applicative ((<|>))
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.Environment  (getEnv)

discordToken :: IO T.Text
discordToken = T.pack <$> getEnv "DIS_TOKEN" <|> TIO.readFile "./token.nightly.secret"

youtubeKey :: IO T.Text
youtubeKey = T.pack <$> getEnv "GOO_KEY" <|> TIO.readFile "./google.secret"

translateKey :: IO T.Text
translateKey = T.pack <$> getEnv "GOO_KEY" <|> TIO.readFile "./google.secret"
