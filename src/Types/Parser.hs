module Types.Parser where

import           Parser.Parser (Parser, runParser)
import           Types.Discord

class Monad m => Par m where
    par :: Parser a -> m (Maybe a)

instance Par DiscordHandler where
    par p = return $ do
        -- get text here
        fst <$> runParser p ""
