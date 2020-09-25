{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Reactions (reactionSwitch) where

import           Network.HTTP.Req
import           Web.Google.Translate      (Lang (..), Source (..), Target (..))

import           Control.Monad.Combinators (empty)

import           UnliftIO

import           Data.Aeson
import qualified Data.Aeson.Types          as JSON (parse)
import           Data.Emoji                (unicodeByName)
import qualified Data.HashMap.Strict       as H (HashMap, lookup)
import           Data.Map                  (elemAt, (!?))
import qualified Data.Text                 as T
import qualified Data.Vector               as V (head)

import           System.Random

import           Discord
import           Discord.Internal.Rest
import qualified Discord.Requests          as R
import           Discord.Types

import           Parser                    (CommandData (args, name))
import           Secrets                   (yt_key)
import           Translate                 (langNames, langTypes, translate)

reactionSwitch :: ReactionInfo -> DiscordHandler ()
reactionSwitch r = if
    | is "symbols"          -> m >>= (trans Nothing $ Just English)
    | is "world_map"        -> m >>= (trans Nothing Nothing) -- \128506\65039
    | otherwise             -> pure ()
    where is e = case unicodeByName e of Just (x:xs) -> x == (T.head $ emojiName $ reactionEmoji r)
                                         Nothing -> False
          m = do
              Right m' <- restCall $ R.GetChannelMessage (reactionChannelId r, reactionMessageId r)
              return m'

trans :: Maybe Lang -> Maybe Lang -> Message -> DiscordHandler ()
trans sl tl' m = do
    let mt = messageText m
    let au = messageAuthor m
    let ch = messageChannel m

    liftIO $ print "transing"

    i <- liftIO $ getStdRandom $ randomR (0, length langTypes)
    let tl = case tl' of Just l  -> l
                         Nothing -> (langTypes !! i)

    t <- liftIO $ translate mt (Source <$> sl) (Target tl)
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

                                to  = EmbedField { embedFieldName = ln $ Just $ T.pack $ show tl
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
