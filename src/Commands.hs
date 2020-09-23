{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Commands where

import Network.HTTP.Req

import Control.Monad.Combinators (empty)

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V (head)
import qualified Data.HashMap.Strict as H

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Parser

m !? k = H.lookup k m

commandSwitch :: CommandData -> Message -> DiscordHandler ()
commandSwitch d m = if
    | name d == "ping" -> ping $ messageChannel m
    | name d == "perish" -> stopDiscord
    | name d == "bless" -> bless $ messageChannel m
    | otherwise -> pure ()
    where n = name d

ping :: ChannelId -> DiscordHandler()
ping c = do
    restCall $ R.CreateMessage c "pong"
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