{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Discord where

import           Control.Monad.Reader       (MonadReader, lift, runReaderT)
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Discord                    as D
import qualified Discord.Internal.Types     as D

newtype DiscordHandler a = DiscordHandler { extractHandler :: ReaderT D.Message D.DiscordHandler a }
    deriving (Functor, Applicative, Monad, MonadFail, MonadReader D.Message)

interpret :: D.Message -> DiscordHandler a -> D.DiscordHandler a
interpret m dh = runReaderT (extractHandler dh) m

liftDH :: D.DiscordHandler a -> DiscordHandler a
liftDH = DiscordHandler . lift
