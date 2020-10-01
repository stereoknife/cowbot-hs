{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Reactions (runReaction, reactionSwitch) where

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

import           Control.Monad             (join)
import           Control.Monad             (when)
import           Control.Monad.Reader      (ReaderT, asks, lift, runReaderT)
import           Secrets                   (yt_key)
import           Translate                 (Trans (..), sendEmbed, translate)

type Reaction = ReaderT ReactionInfo DiscordHandler ()

runReaction :: ReaderT r m a -> r -> m a
runReaction = runReaderT

reactionSwitch :: Reaction
reactionSwitch = do
    mid <- asks reactionMessageId
    cid <- asks reactionChannelId
    em  <- asks $ emojiName . reactionEmoji
    amt <- lift $ do
            em <- restCall $ R.GetReactions (cid, mid) em (2, R.BeforeReaction mid)
            case em of Right m -> return $ m
                       _       -> return []

    let is e = T.head e == T.head em
    if
        | length amt > 1 -> pure ()
        | is "🔣"   -> reactTranslate $ Just English
        | is "🗺"   -> reactTranslate Nothing
        | otherwise -> pure ()
    return ()

reactTranslate :: Maybe Lang -> Reaction
reactTranslate to = do
    mid <- asks reactionMessageId
    cid <- asks reactionChannelId
    em  <- asks $ emojiName . reactionEmoji
    msg <- lift $ do
            restCall $ R.CreateReaction (cid, mid) em
            em <- restCall $ R.GetChannelMessage (cid, mid)
            case em of Right m -> return $ Just m
                       _       -> return empty

    transl <- case messageText <$> msg
        of Just mt -> liftIO $
                translate mt Nothing $ to
           _ -> empty

    case (transl, messageAuthor <$> msg)
        of (Trans { fromText = Just ft
                  , fromLang = Just fl
                  , toText = Just tt
                  , toLang = Just tl
                  , success = True
                  }
                , Just ma) -> do
                    lift $ restCall $ if ft == tt then R.CreateMessage cid "Nothing to translate.."
                                                  else R.CreateMessageEmbed cid T.empty $ sendEmbed ma (fl, ft) (tl, tt)
                    return ()
           _ -> return ()
