{-# LANGUAGE TypeApplications #-}
module Commands.Translate ( translate
                          ) where


import           Control.Monad        (guard)
import           Control.Monad.Reader (MonadIO (liftIO), asks)
import           Data.Bot             (Command)
import           Data.Discord         (Exposes (asksExposed))
import           Data.Language        (Lang (English))
import           Data.Text            (pack)
import qualified Data.Text.Lazy       as T (Text, null, toStrict)
import           Data.Translation     (LocalizedText (Auto), locText,
                                       locTextArray, whatLang)
import qualified Data.Translation     as T (translate)
import           Discord              (def)
import           Discord.Types        (CreateEmbed (createEmbedAuthorIcon, createEmbedAuthorName, createEmbedFields),
                                       CreateEmbedImage (CreateEmbedImageUrl),
                                       EmbedField (EmbedField, embedFieldInline, embedFieldName, embedFieldValue),
                                       Message (messageAuthor),
                                       User (userAvatar, userId, userName))
import           Network.Discord      (embed)

translate :: T.Text -> Command ()
translate m = do
    guard $ not $ T.null m

    let tx = Auto m
    tr <- T.translate tx English
    a <- asksExposed @Message messageAuthor

    let trr = take 2 $ locTextArray tr

    liftIO $ print "translating"
    liftIO $ print $ show tr
    liftIO $ print "translated"

    embed $ let fr = EmbedField { embedFieldName = pack $ show $ whatLang $ head trr
                                , embedFieldValue = T.toStrict $ locText $ head trr
                                , embedFieldInline = Just False
                                }

                to = EmbedField { embedFieldName = pack $ show $ whatLang $ last trr
                                , embedFieldValue = T.toStrict $ locText $ last trr
                                , embedFieldInline = Just False
                                }

                pic = do
                        av <- userAvatar a
                        pure $ CreateEmbedImageUrl $
                            "https://cdn.discordapp.com/avatars/"
                            <> (pack . show $ userId a)
                            <> "/" <> av <> ".png"

                in def { createEmbedAuthorName = userName a
                        , createEmbedFields     = [fr, to]
                        , createEmbedAuthorIcon = pic
                        }

    return ()
