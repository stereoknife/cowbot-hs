module Secrets ( token
               , yt_key
               , tr_key
               , botAdmins
               ) where

import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Discord.Types      (UserId)
import           GHC.Base           (Alternative ((<|>)))
import           System.Environment (getEnv)
import qualified System.IO          as IO

token :: IO T.Text
token = T.pack <$> getEnv "DIS_TOKEN" <|> TIO.readFile "./token.secret"

yt_key :: IO T.Text
yt_key = T.pack <$> getEnv "GOO_KEY" <|> TIO.readFile "./google.secret"

tr_key :: IO T.Text
tr_key = T.pack <$> getEnv "GOO_KEY" <|> TIO.readFile "./google.secret"

botAdmins :: IO [UserId]
botAdmins = do
        ids <- words <$> IO.readFile "./admins.secret"
        return $ read <$> ids
