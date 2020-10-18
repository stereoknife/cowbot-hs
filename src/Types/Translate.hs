module Types.Translate where

import           Data.Text            (Text)
import           Web.Google.Translate (Source, Target)

type Message = Text

data TranslateResult = Fail
                     | NoResult
                     | Success { fromText :: Text
                               , fromLang :: Text
                               , toText   :: Text
                               , toLang   :: Text
                               } deriving (Show)

class Monad m => Translate m where
  translate :: Source -> Target -> Message -> m TranslateResult
