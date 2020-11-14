{-# LANGUAGE OverloadedStrings #-}

module Types.Translate where

import           Data.Text (Text)

fromLang :: Lang -> Text
fromLang l = case l of
  Afrikaans          -> "af"
  Albanian           -> "sq"
  Arabic             -> "ar"
  Armenian           -> "hy"
  Azerbaijani        -> "az"
  Basque             -> "eu"
  Belarusian         -> "be"
  Bengali            -> "bn"
  Bosnian            -> "bs"
  Bulgarian          -> "bg"
  Catalan            -> "ca"
  Cebuano            -> "ceb"
  Chichewa           -> "ny"
  ChineseSimplified  -> "zh"
  ChineseTraditional -> "zh-TW"
  Croatian           -> "hr"
  Czech              -> "cs"
  Danish             -> "da"
  Dutch              -> "nl"
  English            -> "en"
  Esperanto          -> "eo"
  Estonian           -> "et"
  Filipino           -> "tl"
  Finnish            -> "fi"
  French             -> "fr"
  Galician           -> "gl"
  Georgian           -> "ka"
  German             -> "de"
  Greek              -> "el"
  Gujarati           -> "gu"
  HaitianCreole      -> "ht"
  Hausa              -> "ha"
  Hebrew             -> "iw"
  Hindi              -> "hi"
  Hmong              -> "hmn"
  Hungarian          -> "hu"
  Icelandic          -> "is"
  Igbo               -> "ig"
  Indonesian         -> "id"
  Irish              -> "ga"
  Italian            -> "it"
  Japanese           -> "ja"
  Javanese           -> "jw"
  Kannada            -> "kn"
  Kazakh             -> "kk"
  Khmer              -> "km"
  Korean             -> "ko"
  Lao                -> "lo"
  Latin              -> "la"
  Latvian            -> "lv"
  Lithuanian         -> "lt"
  Macedonian         -> "mk"
  Malagasy           -> "mg"
  Malay              -> "ms"
  Malayalam          -> "ml"
  Maltese            -> "mt"
  Maori              -> "mi"
  Marathi            -> "mr"
  Mongolian          -> "mn"
  MyanmarBurmese     -> "my"
  Nepali             -> "ne"
  Norwegian          -> "no"
  Persian            -> "fa"
  Polish             -> "pl"
  Portuguese         -> "pt"
  Punjabi            -> "pa"
  Romanian           -> "ro"
  Russian            -> "ru"
  Serbian            -> "sr"
  Sesotho            -> "st"
  Sinhala            -> "si"
  Slovak             -> "sk"
  Slovenian          -> "sl"
  Somali             -> "so"
  Spanish            -> "es"
  Sundanese          -> "su"
  Swahili            -> "sw"
  Swedish            -> "sv"
  Tajik              -> "tg"
  Tamil              -> "ta"
  Telugu             -> "te"
  Thai               -> "th"
  Turkish            -> "tr"
  Ukrainian          -> "uk"
  Urdu               -> "ur"
  Uzbek              -> "uz"
  Vietnamese         -> "vi"
  Welsh              -> "cy"
  Yiddish            -> "yi"
  Yoruba             -> "yo"
  Zulu               -> "zu"

fromShortcode l = case l of
  "af"    -> Afrikaans
  "sq"    -> Albanian
  "ar"    -> Arabic
  "hy"    -> Armenian
  "az"    -> Azerbaijani
  "eu"    -> Basque
  "be"    -> Belarusian
  "bn"    -> Bengali
  "bs"    -> Bosnian
  "bg"    -> Bulgarian
  "ca"    -> Catalan
  "ceb"   -> Cebuano
  "ny"    -> Chichewa
  "zh"    -> ChineseSimplified
  "zh-TW" -> ChineseTraditional
  "hr"    -> Croatian
  "cs"    -> Czech
  "da"    -> Danish
  "nl"    -> Dutch
  "en"    -> English
  "eo"    -> Esperanto
  "et"    -> Estonian
  "tl"    -> Filipino
  "fi"    -> Finnish
  "fr"    -> French
  "gl"    -> Galician
  "ka"    -> Georgian
  "de"    -> German
  "el"    -> Greek
  "gu"    -> Gujarati
  "ht"    -> HaitianCreole
  "ha"    -> Hausa
  "iw"    -> Hebrew
  "hi"    -> Hindi
  "hmn"   -> Hmong
  "hu"    -> Hungarian
  "is"    -> Icelandic
  "ig"    -> Igbo
  "id"    -> Indonesian
  "ga"    -> Irish
  "it"    -> Italian
  "ja"    -> Japanese
  "jw"    -> Javanese
  "kn"    -> Kannada
  "kk"    -> Kazakh
  "km"    -> Khmer
  "ko"    -> Korean
  "lo"    -> Lao
  "la"    -> Latin
  "lv"    -> Latvian
  "lt"    -> Lithuanian
  "mk"    -> Macedonian
  "mg"    -> Malagasy
  "ms"    -> Malay
  "ml"    -> Malayalam
  "mt"    -> Maltese
  "mi"    -> Maori
  "mr"    -> Marathi
  "mn"    -> Mongolian
  "my"    -> MyanmarBurmese
  "ne"    -> Nepali
  "no"    -> Norwegian
  "fa"    -> Persian
  "pl"    -> Polish
  "pt"    -> Portuguese
  "pa"    -> Punjabi
  "ro"    -> Romanian
  "ru"    -> Russian
  "sr"    -> Serbian
  "st"    -> Sesotho
  "si"    -> Sinhala
  "sk"    -> Slovak
  "sl"    -> Slovenian
  "so"    -> Somali
  "es"    -> Spanish
  "su"    -> Sundanese
  "sw"    -> Swahili
  "sv"    -> Swedish
  "tg"    -> Tajik
  "ta"    -> Tamil
  "te"    -> Telugu
  "th"    -> Thai
  "tr"    -> Turkish
  "uk"    -> Ukrainian
  "ur"    -> Urdu
  "uz"    -> Uzbek
  "vi"    -> Vietnamese
  "cy"    -> Welsh
  "yi"    -> Yiddish
  "yo"    -> Yoruba
  "zu"    -> Zulu


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
          deriving (Show, Read, Eq, Ord)

count = 90
