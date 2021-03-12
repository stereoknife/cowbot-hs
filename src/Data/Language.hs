module Data.Language ( Lang (..)
                     , toShortCode
                     , toShortCodeT
                     , fromShortCode
                     , fromShortCodeT
                     ) where

import           Data.Aeson          (FromJSON (..), ToJSON (..),
                                      Value (String), withText)
import           Data.Aeson.Encoding (string)
import qualified Data.Text           as T (pack, unpack)
import qualified Data.Text.Lazy      as L (Text, pack, unpack)

-- Lang definition
data Lang = Afrikaans
          | Albanian
          | Arabic
          | Armenian
          | Azerbaijani
          | Basque
          | Belarusian
          | Bengali
          | Bosnian
          | Bulgarian
          | Catalan
          | Cebuano
          | Chichewa
          | ChineseSimplified
          | ChineseTraditional
          | Croatian
          | Czech
          | Danish
          | Dutch
          | English
          | Esperanto
          | Estonian
          | Filipino
          | Finnish
          | French
          | Galician
          | Georgian
          | German
          | Greek
          | Gujarati
          | HaitianCreole
          | Hausa
          | Hebrew
          | Hindi
          | Hmong
          | Hungarian
          | Icelandic
          | Igbo
          | Indonesian
          | Irish
          | Italian
          | Japanese
          | Javanese
          | Kannada
          | Kazakh
          | Khmer
          | Korean
          | Lao
          | Latin
          | Latvian
          | Lithuanian
          | Macedonian
          | Malagasy
          | Malay
          | Malayalam
          | Maltese
          | Maori
          | Marathi
          | Mongolian
          | MyanmarBurmese
          | Nepali
          | Norwegian
          | Persian
          | Polish
          | Portuguese
          | Punjabi
          | Romanian
          | Russian
          | Serbian
          | Sesotho
          | Sinhala
          | Slovak
          | Slovenian
          | Somali
          | Spanish
          | Sundanese
          | Swahili
          | Swedish
          | Tajik
          | Tamil
          | Telugu
          | Thai
          | Turkish
          | Ukrainian
          | Urdu
          | Uzbek
          | Vietnamese
          | Welsh
          | Yiddish
          | Yoruba
          | Zulu
          deriving (Show, Eq, Ord, Enum)

instance FromJSON Lang where
  parseJSON = withText "Lang" $ \t -> case fromShortCode $ T.unpack t of Just t' -> pure t'
                                                                         Nothing -> fail "Error when pasing JSON Lang"

instance ToJSON Lang where
  toJSON = String . T.pack . toShortCode
  toEncoding = string . toShortCode

toShortCode :: Lang -> String
toShortCode Afrikaans          = "af"
toShortCode Albanian           = "sq"
toShortCode Arabic             = "ar"
toShortCode Armenian           = "hy"
toShortCode Azerbaijani        = "az"
toShortCode Basque             = "eu"
toShortCode Belarusian         = "be"
toShortCode Bengali            = "bn"
toShortCode Bosnian            = "bs"
toShortCode Bulgarian          = "bg"
toShortCode Catalan            = "ca"
toShortCode Cebuano            = "ceb"
toShortCode Chichewa           = "ny"
toShortCode ChineseSimplified  = "zh"
toShortCode ChineseTraditional = "zh-TW"
toShortCode Croatian           = "hr"
toShortCode Czech              = "cs"
toShortCode Danish             = "da"
toShortCode Dutch              = "nl"
toShortCode English            = "en"
toShortCode Esperanto          = "eo"
toShortCode Estonian           = "et"
toShortCode Filipino           = "tl"
toShortCode Finnish            = "fi"
toShortCode French             = "fr"
toShortCode Galician           = "gl"
toShortCode Georgian           = "ka"
toShortCode German             = "de"
toShortCode Greek              = "el"
toShortCode Gujarati           = "gu"
toShortCode HaitianCreole      = "ht"
toShortCode Hausa              = "ha"
toShortCode Hebrew             = "iw"
toShortCode Hindi              = "hi"
toShortCode Hmong              = "hmn"
toShortCode Hungarian          = "hu"
toShortCode Icelandic          = "is"
toShortCode Igbo               = "ig"
toShortCode Indonesian         = "id"
toShortCode Irish              = "ga"
toShortCode Italian            = "it"
toShortCode Japanese           = "ja"
toShortCode Javanese           = "jw"
toShortCode Kannada            = "kn"
toShortCode Kazakh             = "kk"
toShortCode Khmer              = "km"
toShortCode Korean             = "ko"
toShortCode Lao                = "lo"
toShortCode Latin              = "la"
toShortCode Latvian            = "lv"
toShortCode Lithuanian         = "lt"
toShortCode Macedonian         = "mk"
toShortCode Malagasy           = "mg"
toShortCode Malay              = "ms"
toShortCode Malayalam          = "ml"
toShortCode Maltese            = "mt"
toShortCode Maori              = "mi"
toShortCode Marathi            = "mr"
toShortCode Mongolian          = "mn"
toShortCode MyanmarBurmese     = "my"
toShortCode Nepali             = "ne"
toShortCode Norwegian          = "no"
toShortCode Persian            = "fa"
toShortCode Polish             = "pl"
toShortCode Portuguese         = "pt"
toShortCode Punjabi            = "pa"
toShortCode Romanian           = "ro"
toShortCode Russian            = "ru"
toShortCode Serbian            = "sr"
toShortCode Sesotho            = "st"
toShortCode Sinhala            = "si"
toShortCode Slovak             = "sk"
toShortCode Slovenian          = "sl"
toShortCode Somali             = "so"
toShortCode Spanish            = "es"
toShortCode Sundanese          = "su"
toShortCode Swahili            = "sw"
toShortCode Swedish            = "sv"
toShortCode Tajik              = "tg"
toShortCode Tamil              = "ta"
toShortCode Telugu             = "te"
toShortCode Thai               = "th"
toShortCode Turkish            = "tr"
toShortCode Ukrainian          = "uk"
toShortCode Urdu               = "ur"
toShortCode Uzbek              = "uz"
toShortCode Vietnamese         = "vi"
toShortCode Welsh              = "cy"
toShortCode Yiddish            = "yi"
toShortCode Yoruba             = "yo"
toShortCode Zulu               = "zu"

toShortCodeT :: Lang -> L.Text
toShortCodeT = L.pack . toShortCode

fromShortCode :: String -> Maybe Lang
fromShortCode "af"    = Just Afrikaans
fromShortCode "sq"    = Just Albanian
fromShortCode "ar"    = Just Arabic
fromShortCode "hy"    = Just Armenian
fromShortCode "az"    = Just Azerbaijani
fromShortCode "eu"    = Just Basque
fromShortCode "be"    = Just Belarusian
fromShortCode "bn"    = Just Bengali
fromShortCode "bs"    = Just Bosnian
fromShortCode "bg"    = Just Bulgarian
fromShortCode "ca"    = Just Catalan
fromShortCode "ceb"   = Just Cebuano
fromShortCode "ny"    = Just Chichewa
fromShortCode "zh"    = Just ChineseSimplified
fromShortCode "zh-TW" = Just ChineseTraditional
fromShortCode "hr"    = Just Croatian
fromShortCode "cs"    = Just Czech
fromShortCode "da"    = Just Danish
fromShortCode "nl"    = Just Dutch
fromShortCode "en"    = Just English
fromShortCode "eo"    = Just Esperanto
fromShortCode "et"    = Just Estonian
fromShortCode "tl"    = Just Filipino
fromShortCode "fi"    = Just Finnish
fromShortCode "fr"    = Just French
fromShortCode "gl"    = Just Galician
fromShortCode "ka"    = Just Georgian
fromShortCode "de"    = Just German
fromShortCode "el"    = Just Greek
fromShortCode "gu"    = Just Gujarati
fromShortCode "ht"    = Just HaitianCreole
fromShortCode "ha"    = Just Hausa
fromShortCode "iw"    = Just Hebrew
fromShortCode "hi"    = Just Hindi
fromShortCode "hmn"   = Just Hmong
fromShortCode "hu"    = Just Hungarian
fromShortCode "is"    = Just Icelandic
fromShortCode "ig"    = Just Igbo
fromShortCode "id"    = Just Indonesian
fromShortCode "ga"    = Just Irish
fromShortCode "it"    = Just Italian
fromShortCode "ja"    = Just Japanese
fromShortCode "jw"    = Just Javanese
fromShortCode "kn"    = Just Kannada
fromShortCode "kk"    = Just Kazakh
fromShortCode "km"    = Just Khmer
fromShortCode "ko"    = Just Korean
fromShortCode "lo"    = Just Lao
fromShortCode "la"    = Just Latin
fromShortCode "lv"    = Just Latvian
fromShortCode "lt"    = Just Lithuanian
fromShortCode "mk"    = Just Macedonian
fromShortCode "mg"    = Just Malagasy
fromShortCode "ms"    = Just Malay
fromShortCode "ml"    = Just Malayalam
fromShortCode "mt"    = Just Maltese
fromShortCode "mi"    = Just Maori
fromShortCode "mr"    = Just Marathi
fromShortCode "mn"    = Just Mongolian
fromShortCode "my"    = Just MyanmarBurmese
fromShortCode "ne"    = Just Nepali
fromShortCode "no"    = Just Norwegian
fromShortCode "fa"    = Just Persian
fromShortCode "pl"    = Just Polish
fromShortCode "pt"    = Just Portuguese
fromShortCode "pa"    = Just Punjabi
fromShortCode "ro"    = Just Romanian
fromShortCode "ru"    = Just Russian
fromShortCode "sr"    = Just Serbian
fromShortCode "st"    = Just Sesotho
fromShortCode "si"    = Just Sinhala
fromShortCode "sk"    = Just Slovak
fromShortCode "sl"    = Just Slovenian
fromShortCode "so"    = Just Somali
fromShortCode "es"    = Just Spanish
fromShortCode "su"    = Just Sundanese
fromShortCode "sw"    = Just Swahili
fromShortCode "sv"    = Just Swedish
fromShortCode "tg"    = Just Tajik
fromShortCode "ta"    = Just Tamil
fromShortCode "te"    = Just Telugu
fromShortCode "th"    = Just Thai
fromShortCode "tr"    = Just Turkish
fromShortCode "uk"    = Just Ukrainian
fromShortCode "ur"    = Just Urdu
fromShortCode "uz"    = Just Uzbek
fromShortCode "vi"    = Just Vietnamese
fromShortCode "cy"    = Just Welsh
fromShortCode "yi"    = Just Yiddish
fromShortCode "yo"    = Just Yoruba
fromShortCode "zu"    = Just Zulu
fromShortCode _       = Nothing

fromShortCodeT :: L.Text -> Maybe Lang
fromShortCodeT = fromShortCode . L.unpack
