{-# LANGUAGE ConstraintKinds #-}

module Data.Network where


import           Control.Monad.Catch (MonadThrow)
import           UnliftIO            (MonadIO)

type Network m = (MonadIO m, MonadThrow m, MonadFail m)
