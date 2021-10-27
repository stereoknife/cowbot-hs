import Howdy.Parser.Types
import GHC.Base (Applicative)
import Control.Monad.Trans (MonadTrans (..))
import Control.Applicative (Alternative)
import Control.Monad
import Data.Text (Text)

newtype ParserT m a = ParserT { runParserT :: Text -> m (Maybe (a, Text)) }

instance Functor m => Functor (ParserT m) where
    fmap f (ParserT p) = ParserT $ \s -> do
        v <- p s
        pure $ fmap (tmap f) v
        where tmap f (a, b) = (f a, b) 

instance Applicative m => Applicative (ParserT m) where
    pure a = ParserT $ \s -> pure $ Just (a, s)
    (ParserT f) <*> (ParserT m) = ParserT $ fmap (<*>) f <*> m
        

instance Monad m => Alternative (ParserT m)

instance MonadTrans ParserT where
    lift = ParserT . fmap pure
