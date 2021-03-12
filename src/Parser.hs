module Parser ( Parser(..)
              , anyChar
              , char
              , chars
              , string
              , anyString
              )
where
import           Parser.Constructors (anyChar, anyString, char, chars, string)
import           Parser.Types        (Parser (..))
