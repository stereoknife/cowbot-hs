{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Reactions where

import           Control.Applicative   (Alternative)
import           Control.Monad.Reader  (ReaderT, guard, runReaderT)
import qualified Data.Text             as T
import           Discord               (DiscordHandler, def, restCall)
import           Discord.Internal.Rest (Emoji (emojiName),
                                        ReactionInfo (reactionChannelId, reactionEmoji, reactionMessageId))
import qualified Discord.Requests      as R
import           Discord.Types         (CreateEmbed (createEmbedAuthorIcon, createEmbedAuthorName, createEmbedFields),
                                        CreateEmbedImage (CreateEmbedImageUrl),
                                        EmbedField (EmbedField, embedFieldInline, embedFieldName, embedFieldValue),
                                        Message (..),
                                        User (userAvatar, userId, userName))
import           Types                 (DiscordRequest (..),
                                        MessageData (askMessage),
                                        ReactionData (askReaction),
                                        Reply (embed, reply), Translate (..),
                                        TranslateResult (fromLang, fromText, toLang, toText))
import           Web.Google.Translate  (Lang (..), Target (..))

type Reaction = ReaderT ReactionInfo DiscordHandler ()

runReaction :: ReaderT r m a -> r -> m a
runReaction = runReaderT

reactionSwitch :: (Reply m, Translate m, MessageData m, ReactionData m, Alternative m, DiscordRequest m) => m ()
reactionSwitch = do
    mid <- askReaction reactionMessageId
    cid <- askReaction reactionChannelId
    em  <- askReaction $ emojiName . reactionEmoji
    amt <- dis $ do
            em <- restCall $ R.GetReactions (cid, mid) em (2, R.BeforeReaction mid)
            case em of Right m -> return $ m
                       _       -> return []

    guard (length amt <= 1)

    let is e = T.head e == T.head em
    if
        | is "🔣"    -> reactTranslate $ Just English
        | is "🗺"    -> reactTranslate Nothing
        | otherwise -> pure ()

    return ()

{--
reactConvert :: (Reply m, Translate m, MessageReader m) => m ()
reactConvert = do
    mid <- asks reactionMessageId
    cid <- asks reactionChannelId
    msg <- lift $ do
            ms <- restCall $ R.GetChannelMessage (cid, mid)
            case ms of Right m -> return $ Just m
                       _       -> return empty

    ()
    return ()

--}

reactTranslate :: (Reply m, Translate m, MessageData m) => Maybe Lang -> m ()
reactTranslate l = do
    mt <- askMessage messageText
    au <- askMessage messageAuthor

    trans <- translate Nothing (Target <$> l) mt

    case trans of Left  t -> reply t
                  Right t -> embed $ let fr = EmbedField { embedFieldName = fromLang t
                                                         , embedFieldValue = fromText t
                                                         , embedFieldInline = Just False
                                                         }

                                         to = EmbedField { embedFieldName = toLang t
                                                         , embedFieldValue = toText t
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


