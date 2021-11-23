{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Commands.Translate (transCmd, transRec, transRecRandom) where

import           Control.Applicative    (Alternative)
import           Control.Monad          (guard)
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Data              (dataTypeOf)
import           Data.Language          (Lang (English), langName)
import           Data.Random            (randomValue)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Translation       (KL (KLang), LocalizedText (Auto),
                                         locText, locTextArray, translate,
                                         whatLang)
import           Discord                (def)
import           Discord.Types          (CreateEmbed (createEmbedAuthorIcon, createEmbedAuthorName, createEmbedFields),
                                         CreateEmbedImage (CreateEmbedImageUrl),
                                         EmbedField (EmbedField, embedFieldInline, embedFieldName, embedFieldValue),
                                         Message (messageText),
                                         User (userAvatar, userId, userName))
import           Howdy.Action           (CommandRunner, ReactionRunner)
import           Howdy.Context          (Context (ctx, fctx))
import           Howdy.Discord.Class    (MonadReply (reply), embed)
import           Howdy.Parser           (MonadParse (parse), rest)

transCmd :: CommandRunner ()
transCmd = do
    t <- parse rest
    trans t English

transRec :: ReactionRunner ()
transRec = do
    m <- fctx @Message messageText
    trans m English

transRecRandom :: ReactionRunner ()
transRecRandom = do
    m <- fctx @Message messageText
    l <- liftIO $ randomValue @Lang $ dataTypeOf English
    -- reply $ langName l
    trans m l

trans :: (MonadIO m, MonadThrow m, MonadReply m) => Text -> Lang -> m ()
trans t l = do
    let tx = Auto t
    tr <- translate tx l
    a <- ctx @User

    let trr = take 2 $ locTextArray tr
    if locText (last trr) == locText (head trr)
    then reply "nothing to translate.."
    else writeOut a trr

writeOut :: MonadReply m => User -> [LocalizedText 'KLang] -> m ()
writeOut a trr = embed $ let fr = EmbedField { embedFieldName = langName $ whatLang $ last trr
                                    , embedFieldValue = locText $ last trr
                                    , embedFieldInline = Just False
                                    }

                             to = EmbedField { embedFieldName = langName $ whatLang $ head trr
                                    , embedFieldValue = locText $ head trr
                                    , embedFieldInline = Just False
                                    }

                             pic = do
                                av <- userAvatar a
                                pure $ CreateEmbedImageUrl $
                                    "https://cdn.discordapp.com/avatars/"
                                    <> (T.pack . show $ userId a)
                                    <> "/" <> av <> ".png"

                         in def { createEmbedAuthorName = userName a
                                , createEmbedFields     = [fr, to]
                                , createEmbedAuthorIcon = pic
                                }
