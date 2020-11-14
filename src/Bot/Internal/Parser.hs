{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bot.Internal.Parser ( Parser
                            , parse
                            ) where

import           Bot.Internal.Discord (Command)
import           Control.Applicative  (Alternative (empty))
import           Control.Monad.State  (MonadState, get, put)
import           Data.Text            (Text)
import           Parser.Parser        (runParser)
import qualified Parser.Parser        as P (Parser (..))


class MonadState Text m => Parser m where
    parse :: P.Parser a -> m (Maybe a)

instance Parser Command where
    parse p = do
        t <- get
        (a, rest) <- ex $ runParser p t
        put rest
        return $ Just a
        where ex (Just v) = return v
              ex Nothing  = empty
