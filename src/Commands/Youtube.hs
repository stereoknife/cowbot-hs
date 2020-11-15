{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Commands.Youtube where

import           Bot.Internal        (Parser (parse), Reply (reply))
import           Bot.Internal.Net    (Net)
import           Control.Applicative (Alternative)
import           Control.Monad       (guard)
import           Data.Maybe          (fromJust, isJust)
import           Net.Youtube         (youtubeRequest)
import           Parser.Parser       (rest)

yt :: (Reply m, Net m, Alternative m, Parser m) => m ()
yt = do
    query <- parse rest
    guard $ isJust query

    id <- youtubeRequest $ fromJust query

    case id of Right id -> reply $ "https://youtube.com/watch?v=" <> id
               _        -> reply $ "Couldn't find anything 😔"
