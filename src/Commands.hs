{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands (commandSwitch) where

import           Network.HTTP.Req
import           Web.Google.Translate      (Lang (..), Target (..))

import           Control.Monad.Combinators (empty)

import           UnliftIO

import           Data.Aeson
import qualified Data.Aeson.Types          as JSON (parse)
import qualified Data.HashMap.Strict       as H (HashMap, lookup)
import           Data.Map                  ((!?))
import qualified Data.Text                 as T
import qualified Data.Vector               as V (head)

import           Discord
import qualified Discord.Requests          as R
import           Discord.Types

import           Parser                    (CommandData (args, name))
import           Secrets                   (yt_key)
import           Translate                 (langNames, translate)

commandSwitch :: CommandData -> Message -> DiscordHandler ()
commandSwitch d m = if
    | is "clap"             -> clap d ch
    | is "bless"            -> bless ch
    | is "perish"           -> stopDiscord
    | any is
        ["yt", "youtube"]   -> yt d ch
    | any is
        ["t", "translate"]  -> trans d m
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
                    <> "q" =: (T.intercalate " " $ args d)

    pid <- return $ withObject "data" $
      \dat -> do
        items   <- dat    .: "items"
        item    <- return  $  V.head items
        id      <- item   .: "id"
        vid     <- id     .: "videoId"
        return (vid :: T.Text)

    rid <- return $ JSON.parse pid $ responseBody r

    case rid of Success id -> restCall $ R.CreateMessage ch $ "https://youtube.com/watch?v=" <> id
                _          -> restCall $ R.CreateMessage ch "Couldn't find anything 😔"

    pure ()


trans :: CommandData -> Message -> DiscordHandler ()
trans d m = do
    let mt = T.intercalate " " $ args d
    let au = messageAuthor m
    let ch = messageChannel m

    t <- liftIO $ translate mt Nothing $ Target English

    case t of Nothing -> empty
              Just (t, l) -> restCall $
                    if t == mt then R.CreateMessage ch $ "Nothing to translate.."
                    else R.CreateMessageEmbed ch T.empty $
                            let ln lc = case lc >>= (!?) langNames
                                        of Just l' ->  l'
                                           Nothing -> "who knever knows!"

                                fr  = EmbedField { embedFieldName = ln l
                                                 , embedFieldValue = mt
                                                 , embedFieldInline = Just False
                                                 }

                                to  = EmbedField { embedFieldName = "English"
                                                 , embedFieldValue = t
                                                 , embedFieldInline = Just False
                                                 }

                                pic = do av <- userAvatar au
                                         pure $ CreateEmbedImageUrl $
                                                "https://cdn.discordapp.com/avatars/"
                                                <> (T.pack . show $ userId au)
                                                <> "/" <> av <> ".png"

                            in def { createEmbedAuthorName = userName au
                                   , createEmbedFields     = [fr, to]
                                   , createEmbedAuthorIcon = pic
                                   }

    pure ()

--in case you missed it
--icym
