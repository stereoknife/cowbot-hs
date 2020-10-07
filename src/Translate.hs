{-# LANGUAGE OverloadedStrings #-}

module Translate (translate, sendEmbed, Trans(..)) where

import qualified Data.Map                as M
import qualified Data.Text               as T
import           Discord                 (def)
import           Discord.Types           (CreateEmbed,
                                          CreateEmbedImage (CreateEmbedImageUrl),
                                          EmbedField (EmbedField), User,
                                          createEmbedAuthorIcon,
                                          createEmbedAuthorName,
                                          createEmbedFields, embedFieldInline,
                                          embedFieldName, embedFieldValue,
                                          userAvatar, userId, userName)
import           Network.HTTP.Client     (Manager)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Secrets
import           System.Random           (getStdRandom, randomR)
import           Web.Google.Translate    (Body (Body), Key (Key), Lang (..),
                                          Source (Source), Target (Target),
                                          TranslatedText (TranslatedText),
                                          Translation (detectedSourceLanguage, translatedText),
                                          TranslationResponse (translations))
import qualified Web.Google.Translate    as TR (translate)


(!?) :: Ord a => M.Map a b -> a -> Maybe b
(!?) = (M.!?)

manager :: IO Manager
manager = newTlsManager

data Trans = Trans { fromText :: Maybe T.Text
                   , fromLang :: Maybe T.Text
                   , toText   :: Maybe T.Text
                   , toLang   :: Maybe T.Text
                   , success  :: Bool
                   } deriving (Show)

failTrans :: Trans
failTrans = Trans { fromText = Nothing, fromLang = Nothing, toText = Nothing, toLang = Nothing, success = False}

translate :: T.Text -> Maybe Lang -> Maybe Lang -> IO Trans
translate b fr mto = do
    k <- Key <$> tr_key
    m <- manager
    -------------------
    i  <- getStdRandom $ randomR (0, length langTypes)
    to <- return $ case mto of Just l -> l
                               _      -> langTypes !! i

    transResult <- TR.translate m k (Source <$> fr) (Target to) $ Body b

    t <- return $ do
        res <- transResult
        let trans = head $ translations res
            f     = T.pack . show <$> detectedSourceLanguage trans
            t     = (\(TranslatedText t) -> t) $ translatedText trans
        return (t, f)

    case t of Right (text, mLang) -> return $ Trans { fromLang = mLang >>= (langNames !?)
                                                    , fromText = Just b
                                                    , toLang = langNames !? (T.pack $ show to)
                                                    , toText = Just text
                                                    , success = True
                                                    }
              Left _              -> return failTrans



sendEmbed :: User -> (T.Text, T.Text) -> (T.Text, T.Text) -> CreateEmbed
sendEmbed au (fl, ft) (tl, tt) =
    let fr = EmbedField { embedFieldName = fl
                        , embedFieldValue = ft
                        , embedFieldInline = Just False
                        }

        to = EmbedField { embedFieldName = tl
                        , embedFieldValue = tt
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

langNames :: M.Map T.Text T.Text
langNames = M.fromList [ ("af", "Afrikaans")
                    , ("sq", "Albanian")
                    , ("ar", "Arabic")
                    , ("hy", "Armenian")
                    , ("az", "Azerbaijani")
                    , ("eu", "Basque")
                    , ("be", "Belarusian")
                    , ("bn", "Bengali")
                    , ("bs", "Bosnian")
                    , ("bg", "Bulgarian")
                    , ("ca", "Catalan")
                    , ("ceb", "Cebuano")
                    , ("ny", "Chichewa")
                    , ("zh", "ChineseSimplified")
                    , ("zh-TW", "ChineseTraditional")
                    , ("hr", "Croatian")
                    , ("cs", "Czech")
                    , ("da", "Danish")
                    , ("nl", "Dutch")
                    , ("en", "English")
                    , ("eo", "Esperanto")
                    , ("et", "Estonian")
                    , ("tl", "Filipino")
                    , ("fi", "Finnish")
                    , ("fr", "French")
                    , ("gl", "Galician")
                    , ("ka", "Georgian")
                    , ("de", "German")
                    , ("el", "Greek")
                    , ("gu", "Gujarati")
                    , ("ht", "HaitianCreole")
                    , ("ha", "Hausa")
                    , ("iw", "Hebrew")
                    , ("hi", "Hindi")
                    , ("hmn", "Hmong")
                    , ("hu", "Hungarian")
                    , ("is", "Icelandic")
                    , ("ig", "Igbo")
                    , ("id", "Indonesian")
                    , ("ga", "Irish")
                    , ("it", "Italian")
                    , ("ja", "Japanese")
                    , ("jw", "Javanese")
                    , ("kn", "Kannada")
                    , ("kk", "Kazakh")
                    , ("km", "Khmer")
                    , ("ko", "Korean")
                    , ("lo", "Lao")
                    , ("la", "Latin")
                    , ("lv", "Latvian")
                    , ("lt", "Lithuanian")
                    , ("mk", "Macedonian")
                    , ("mg", "Malagasy")
                    , ("ms", "Malay")
                    , ("ml", "Malayalam")
                    , ("mt", "Maltese")
                    , ("mi", "Maori")
                    , ("mr", "Marathi")
                    , ("mn", "Mongolian")
                    , ("my", "MyanmarBurmese")
                    , ("ne", "Nepali")
                    , ("no", "Norwegian")
                    , ("fa", "Persian")
                    , ("pl", "Polish")
                    , ("pt", "Portuguese")
                    , ("pa", "Punjabi")
                    , ("ro", "Romanian")
                    , ("ru", "Russian")
                    , ("sr", "Serbian")
                    , ("st", "Sesotho")
                    , ("si", "Sinhala")
                    , ("sk", "Slovak")
                    , ("sl", "Slovenian")
                    , ("so", "Somali")
                    , ("es", "Spanish")
                    , ("su", "Sundanese")
                    , ("sw", "Swahili")
                    , ("sv", "Swedish")
                    , ("tg", "Tajik")
                    , ("ta", "Tamil")
                    , ("te", "Telugu")
                    , ("th", "Thai")
                    , ("tr", "Turkish")
                    , ("uk", "Ukrainian")
                    , ("ur", "Urdu")
                    , ("uz", "Uzbek")
                    , ("vi", "Vietnamese")
                    , ("cy", "Welsh")
                    , ("yi", "Yiddish")
                    , ("yo", "Yoruba")
                    , ("zu", "Zulu")
                    ]

langTypes :: [ Lang ]
langTypes = [ Afrikaans
            , Albanian
            , Arabic
            , Armenian
            , Azerbaijani
            , Basque
            , Belarusian
            , Bengali
            , Bosnian
            , Bulgarian
            , Catalan
            , Cebuano
            , Chichewa
            , ChineseSimplified
            , ChineseTraditional
            , Croatian
            , Czech
            , Danish
            , Dutch
            , English
            , Esperanto
            , Estonian
            , Filipino
            , Finnish
            , French
            , Galician
            , Georgian
            , German
            , Greek
            , Gujarati
            , HaitianCreole
            , Hausa
            , Hebrew
            , Hindi
            , Hmong
            , Hungarian
            , Icelandic
            , Igbo
            , Indonesian
            , Irish
            , Italian
            , Japanese
            , Javanese
            , Kannada
            , Kazakh
            , Khmer
            , Korean
            , Lao
            , Latin
            , Latvian
            , Lithuanian
            , Macedonian
            , Malagasy
            , Malay
            , Malayalam
            , Maltese
            , Maori
            , Marathi
            , Mongolian
            , MyanmarBurmese
            , Nepali
            , Norwegian
            , Persian
            , Polish
            , Portuguese
            , Punjabi
            , Romanian
            , Russian
            , Serbian
            , Sesotho
            , Sinhala
            , Slovak
            , Slovenian
            , Somali
            , Spanish
            , Sundanese
            , Swahili
            , Swedish
            , Tajik
            , Tamil
            , Telugu
            , Thai
            , Turkish
            , Ukrainian
            , Urdu
            , Uzbek
            , Vietnamese
            , Welsh
            , Yiddish
            , Yoruba
            , Zulu
            ]
