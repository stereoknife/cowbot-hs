{-# LANGUAGE OverloadedStrings #-}

module Types.Translate where

import           Control.Applicative     (Alternative ((<|>)))
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Data.Bifunctor          (Bifunctor (first))
import           Data.Map                ((!?))
import qualified Data.Map                as M
import           Data.Text               (Text, pack)
import           Network.HTTP.Client     (Manager)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Secrets                 (tr_key)
import           System.Random           (getStdRandom, randomR)
import           Types.Discord           (DiscordHandler)
import           Web.Google.Translate    (Body (..), Key (..), Lang (..),
                                          Source (..), Target (..),
                                          TranslatedText (..),
                                          detectedSourceLanguage,
                                          translatedText, translations)
import qualified Web.Google.Translate    as TR (translate)
type Message = Text

manager :: IO Manager
manager = newTlsManager

data TranslateResult = Result { fromText :: Text
                              , fromLang :: Text
                              , toText   :: Text
                              , toLang   :: Text
                              } deriving (Show)

class Monad m => Translate m where
  translate :: Maybe Source -> Maybe Target -> Message -> m (Either Text TranslateResult)

instance Translate DiscordHandler where
    translate s t' b = do
        k <- liftIO $ Key <$> tr_key
        m <- liftIO $ manager
        -------------------
        i <- liftIO $ getStdRandom $ randomR (0, length langTypes)
        t <- return $ case t' of Just l -> l
                                 _      -> Target $ langTypes !! i

        transResult <- liftIO $ TR.translate m k s t $ Body b

        return $ do
              res              <- first (const "katastrooffi") $ head . translations <$> transResult
              f                <- return . extract
                                         $ pack . show . unSource <$> s
                                        <|> pack . show <$> detectedSourceLanguage res
                                        <|> Just "who knever knows.."
              TranslatedText r <- return $ translatedText res
              if r == b
              then Left "nothing to translate.."
              else return $ Result { fromText = b
                                    , fromLang = extract $ langNames !? f <|> Just f
                                    , toText = r
                                    , toLang = extract $ let tl = unTarget t
                                                         in (langNames !? (pack $ show tl)) <|> (Just $ pack $ show tl)
                                    }

        where extract (Just a) = a
              unSource (Source a) = a
              unTarget (Target a) = a

langNames :: M.Map Text Text
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
                    , ("zh", "Simplified Chinese")
                    , ("zh-TW", "Traditional Chinese")
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
                    , ("ht", "Haitian Creole")
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
                    , ("my", "Myanmar/Burmese")
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
