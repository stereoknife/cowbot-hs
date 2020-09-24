module Secrets ( token
               , yt_key
               ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

token :: IO T.Text
token = TIO.readFile "./token.secret"

yt_key :: IO T.Text
yt_key = TIO.readFile "./yt.secret"