{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Commands.Translate ( translate
                          ) where

import           Bot.Internal  (MessageData (..), Parser (parse),
                                Reply (embed, reply))
import           Data.Maybe    (fromMaybe)
import           Data.Text     (intercalate, pack)
import           Discord       (def)
import           Discord.Types (CreateEmbed (createEmbedAuthorIcon, createEmbedAuthorName, createEmbedFields),
                                CreateEmbedImage (CreateEmbedImageUrl),
                                EmbedField (EmbedField, embedFieldInline, embedFieldName, embedFieldValue),
                                Message (messageAuthor),
                                User (userAvatar, userId, userName))
import           Net.Translate (Translate, TranslationResponse (..),
                                translateRequest)
import           Parser.Parser (arg, args, flag)

translate :: (Reply m, Translate m, MessageData m, Parser m) => m ()
translate = do
    f <- parse (flag "from" >> arg)
    t <- parse (flag "to" >> arg)

    m <- parse args
    a <- askMessage messageAuthor

    trans <- case m of Just m  -> translateRequest f (fromMaybe "en" t) $ intercalate " " m
                       Nothing -> return $ Left "nothing to translate.."

    case trans of Left  t -> reply $ pack t
                  Right t -> embed $ let fr = EmbedField { embedFieldName = fromLang t
                                                         , embedFieldValue = fromText t
                                                         , embedFieldInline = Just False
                                                         }

                                         to = EmbedField { embedFieldName = toLang t
                                                         , embedFieldValue = toText t
                                                         , embedFieldInline = Just False
                                                         }

                                         pic = do av <- userAvatar a
                                                  pure $ CreateEmbedImageUrl $
                                                      "https://cdn.discordapp.com/avatars/"
                                                      <> (pack . show $ userId a)
                                                      <> "/" <> av <> ".png"

                                     in def { createEmbedAuthorName = userName a
                                           , createEmbedFields     = [fr, to]
                                           , createEmbedAuthorIcon = pic
                                           }

    return ()
