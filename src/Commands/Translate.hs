{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Commands.Translate ( comTranslate
                          ) where

import           Control.Applicative  (Alternative ((<|>)))
import           Data.Aeson           (FromJSON (parseJSON), Value (String))
import           Data.Aeson.Types     (parseMaybe)
import           Data.Text            (intercalate)
import qualified Data.Text            as T
import           Discord              (def)
import           Discord.Types        (CreateEmbed (createEmbedAuthorIcon, createEmbedAuthorName, createEmbedFields),
                                       CreateEmbedImage (CreateEmbedImageUrl),
                                       EmbedField (EmbedField, embedFieldInline, embedFieldName, embedFieldValue),
                                       Message (messageAuthor),
                                       User (userAvatar, userId, userName))
import           Parser.Parser        (arg, args, flag)
import           Types                (MessageData (..), Parser (parse),
                                       Reply (embed, reply), Translate (..),
                                       TranslateResult (fromLang, fromText, toLang, toText))
import           Web.Google.Translate (Lang (..), Source (..), Target (..))

data Signal a = No | Next | Final a

comTranslate :: (Reply m, Translate m, MessageData m, Parser m) => m ()
comTranslate = do
    f' <- parse (flag "from" >> arg)
    t' <- parse (flag "to" >> arg)

    m <- parse args
    a <- askMessage messageAuthor

    f <- return $  f' >>= parseMaybe (parseJSON @Lang) . String
    t <- return $ (t' >>= parseMaybe (parseJSON @Lang) . String) <|> Just English

    trans <- case m of Just m  -> translate (Source <$> f) (Target <$> t) $ intercalate " " m
                       Nothing -> return $ Left "nothing to translate.."

    case trans of Left  t -> reply t
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
                                                      <> (T.pack . show $ userId a)
                                                      <> "/" <> av <> ".png"

                                     in def { createEmbedAuthorName = userName a
                                           , createEmbedFields     = [fr, to]
                                           , createEmbedAuthorIcon = pic
                                           }

    return ()
