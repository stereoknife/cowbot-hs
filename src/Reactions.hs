{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Reactions where


import           Bot.Internal     (MessageData (askMessage),
                                   Reply (embed, reply))
import           Bot.Internal.Net (Net)
import           Data.Maybe       (fromMaybe)
import qualified Data.Text        as T
import           Discord          (def)
import           Discord.Types    (CreateEmbed (createEmbedAuthorIcon, createEmbedAuthorName, createEmbedFields),
                                   CreateEmbedImage (CreateEmbedImageUrl),
                                   EmbedField (EmbedField, embedFieldInline, embedFieldName, embedFieldValue),
                                   Message (..),
                                   User (userAvatar, userId, userName))
import           Net.Translate    (fromLang, fromText, toLang, toText,
                                   translateRequest)
import           Types.Translate  (Lang, randomLang)

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

(<?>) :: Maybe c -> c -> c
(<?>) = flip fromMaybe

reactTranslate :: (Reply m, Net m, MessageData m) => Maybe Lang -> m ()
reactTranslate l = do
    mt <- askMessage messageText
    au <- askMessage messageAuthor
    rl <- randomLang

    trans <- translateRequest Nothing (l <?> rl) mt

    case trans of Left  t -> reply $ T.pack t
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


