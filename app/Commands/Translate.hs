{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE TypeApplications   #-}

module Commands.Translate where
import           Control.Monad.Reader    (MonadIO (..), MonadReader (ask))
import           Data.Data               (Data (..))
import           Data.Language           (Lang (English), langName)
import           Data.Random             (randomValue)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Translation        (KL (KLang), LocalizedText (Auto),
                                          locText, locTextArray, translate,
                                          whatLang)
import           Discord                 (def)
import           Discord.Types           (CreateEmbed (createEmbedAuthorIcon, createEmbedAuthorName, createEmbedFields),
                                          CreateEmbedImage (CreateEmbedImageUrl),
                                          EmbedField (EmbedField, embedFieldInline, embedFieldName, embedFieldValue),
                                          User (userAvatar, userId, userName))
import           Howdy.Comptime.Command  (Command, CommandInput (..))
import           Howdy.Comptime.Reaction (Reaction, ReactionInput (..))
import           Howdy.Internal.Discord  (MonadReply, embed, send)


transCmd :: Command
transCmd c = do
    res <- trans c.args English
    case res of
        Left t  -> send t
        Right t -> writeOut c.author t

transRec :: Reaction
transRec r = do
    res <- trans r.args English
    case res of
        Left t  -> send t
        Right t -> writeOut r.author t

transRecRandom :: Reaction
transRecRandom r = do
    l <- liftIO $ randomValue @Lang $ dataTypeOf English
    res <- trans r.args l
    case res of
        Left t  -> send t
        Right t -> writeOut r.author t

trans :: (MonadIO m) => Text -> Lang -> m (Either Text [LocalizedText 'KLang])
trans t l = do
    let tx = Auto t
    tr <- liftIO $ translate tx l

    let trr = take 2 $ locTextArray tr
    if locText (last trr) == locText (head trr)
    then pure $ Left "nothing to translate.."
    else pure $ Right trr

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
                                pure $ CreateEmbedImageUrl
                                     $ "https://cdn.discordapp.com/avatars/"
                                     <> (T.pack . show $ userId a)
                                     <> "/" <> av <> ".png"

                         in def { createEmbedAuthorName = userName a
                                , createEmbedFields     = [fr, to]
                                , createEmbedAuthorIcon = pic
                                }
