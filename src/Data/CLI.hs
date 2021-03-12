{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}
module Data.CLI where

import           Control.Applicative (Alternative (some))
import qualified Data.Text           as T (Text)
import           Data.Text.Lazy      (Text, toStrict)
import           Parser.Constructors as P (anyString, char, chars, string)
import           Parser.Types        (Parser)
