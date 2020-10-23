module Types.Parser where

import           Control.Monad.State (MonadState, get, put)
import           Data.Text           (Text)
import           Parser.Parser       (runParser)
import qualified Parser.Parser       as P (Parser (..))
import           Types.Discord       (DiscordHandler)

class MonadState Text m => Par m where
    par :: P.Parser a -> m (Maybe a)

instance Par DiscordHandler where
    par p = do
        t <- get
        case runParser p t of Nothing        -> return Nothing
                              Just (r, rest) -> do
                                                put rest
                                                return $ Just r
