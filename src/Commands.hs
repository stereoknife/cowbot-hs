{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Commands where

import Network.HTTP.Req

import Control.Monad.Combinators (empty)

import UnliftIO

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V (head)
import qualified Data.HashMap.Strict as H (lookup, HashMap)

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Parser ( CommandData(name, args))
import Data.Aeson.Types (parse)

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

    o <- case V.head $ (responseBody r :: Array) of
      Object o -> return $ do
        b <- o !? "bookname"
        c <- o !? "chapter"
        v <- o !? "verse"
        t <- o !? "text"
        case (b, c, v, t) of (String b, String c, String v, String t) -> Just (b, c, v, t)
                             _ -> Nothing
      _ -> return Nothing

    case o of Just (b, c, v, t) -> restCall $ R.CreateMessage ch $ "**" <> b <> " " <> c <> ":" <> v <> "** " <> t
              _ -> empty

    pure ()

yt :: CommandData -> ChannelId -> DiscordHandler ()
yt d ch = do
    r <- runReq defaultHttpConfig $ req
       {-Method-} GET
          {-URL-} (https "www.googleapis.com"/:"youtube"/:"v3"/:"search")
         {-Body-} NoReqBody
{-Response type-} jsonResponse
        {-Query-} $ "part" =: ("snippet" :: T.Text)
                    <> "maxResults" =: ("1" :: T.Text)
                    <> "key" =: ("" :: T.Text)
                    <> "q" =: (head $ args d)

    id <- return $ withObject "data" 
      (\dat -> do
        items   <- dat    .: "items"
        item    <- return $ V.head items
        id      <- item   .: "id"
        videoId <- id     .: "videoId"
        return (videoId :: T.Text))

    vid <- return $ parse id $ responseBody r

    case vid of Success v -> restCall $ R.CreateMessage ch $ "https://youtube.com/watch?v=" <> v
                _         -> restCall $ R.CreateMessage ch "Couldn't find anything 😔"

    pure ()