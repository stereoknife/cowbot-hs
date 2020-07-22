{-# LANGUAGE OverloadedStrings #-}

module Src.Constants
( prefixes
) where

import qualified Data.Text as T

prefixes :: [T.Text]
prefixes = [ "!"
           , "pfx"
           ]