{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Commands (commandSwitch) where

import Network.HTTP.Req

import Control.Monad.Combinators (empty)

import UnliftIO

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V (head)
import qualified Data.HashMap.Strict as H (lookup, HashMap)
import qualified Data.Aeson.Types as JSON (parse)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Parser ( CommandData(name, args))
import Secrets (yt_key)

(!?) :: H.HashMap T.Text v -> T.Text -> Maybe v
(!?) = flip H.lookup

commandSwitch :: CommandData -> Message -> DiscordHandler ()
commandSwitch d m = if
    | is "clap" -> clap d ch
    | is "bless" -> bless ch
    | any is ["yt", "youtube"] -> yt d ch
    | is "perish" -> stopDiscord
    | otherwise -> pure ()
    where ch = messageChannel m
          is = (name d ==)

clap :: CommandData -> ChannelId -> DiscordHandler()
clap d c = do
    restCall $ R.CreateMessage c $ (T.intercalate "👏" $ args d) <> "👏"
    pure ()

bless :: ChannelId -> DiscordHandler ()
bless ch = do
    r <- runReq defaultHttpConfig $ req
       {-Method-} GET
          {-URL-} (https "labs.bible.org" /: "api")
         {-Body-} NoReqBody
{-Response type-} jsonResponse
        {-Query-} $ "passage" =: ("random" :: T.Text)
                    <> "type" =: ("json" :: T.Text)

    pb <- return $ withObject "response" $
      \res -> do
        b <- res .: "bookname"
        c <- res .: "chapter"
        v <- res .: "verse"
        t <- res .: "text"
        return ((b, c, v, t) :: (T.Text, T.Text, T.Text, T.Text))

    rb <- return $ JSON.parse pb $ responseBody r

    case rb of Success (b, c, v, t) -> restCall $ R.CreateMessage ch $ "**" <> b <> " " <> c <> ":" <> v <> "** " <> t
               _ -> empty

    pure ()

yt :: CommandData -> ChannelId -> DiscordHandler ()
yt d ch = do
    key <- liftIO yt_key
    r <- runReq defaultHttpConfig $ req
       {-Method-} GET
          {-URL-} (https "www.googleapis.com"/:"youtube"/:"v3"/:"search")
         {-Body-} NoReqBody
{-Response type-} jsonResponse
        {-Query-} $ "part" =: ("snippet" :: T.Text)
                    <> "maxResults" =: ("1" :: T.Text)
                    <> "key" =: key
                    <> "q" =: head (args d)

    pid <- return $ withObject "data" $
      \dat -> do
        items   <- dat    .: "items"
        item    <- return $ V.head items
        id      <- item   .: "id"
        vid     <- id     .: "videoId"
        return (vid :: T.Text)

    rid <- return $ JSON.parse pid $ responseBody r

    case rid of Success id -> restCall $ R.CreateMessage ch $ "https://youtube.com/watch?v=" <> id
                _         -> restCall $ R.CreateMessage ch "Couldn't find anything 😔"

    pure ()
