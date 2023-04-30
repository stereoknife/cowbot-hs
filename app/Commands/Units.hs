{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Commands.Units where

import           Data.Text              (Text)
import           Howdy.Comptime.Command (Command, CommandInput (author))
import           Howdy.Error            (HowdyException (..), report)
import           Howdy.Internal.Discord (send)
import           Text.Parsec

units :: Command
units i = do

    pure ()

-- primitive
filler :: Stream s m Char => ParsecT s () m String
filler = manyTill digit anyChar

num :: Stream s m Char => ParsecT s () m Float
num = read <$> many digit


-- units
inch :: Stream s m Char => ParsecT s () m Length
inch = do
    n <- num
    spaces
    tryString "inches" <|> tryString "inch" <|> tryString "in" <|> string "\""
    pure $ inches n

foot :: Stream s m Char => ParsecT s () m Length
foot = do
    n <- num
    spaces
    tryString "feet" <|> tryString "foot" <|> tryString "ft" <|> string "'"
    pure $ feet n

ftin :: Stream s m Char => ParsecT s () m Length
ftin = do
    f <- foot
    spaces
    n <- num
    spaces
    optional (tryString "inches" <|> tryString "inch" <|> tryString "in" <|> string "\"")
    pure $ inches n + f


-- magnitudes
length :: Stream s m Char => ParsecT s () m Length
length = try inch <|> try foot

-- helpers
tryString :: Stream s m Char => String -> ParsecT s () m String
tryString = try . string

-- types
feet :: Float -> Length
feet = Length . (*) 0.3048

inches :: Float -> Length
inches = Length . (*) 0.0254

newtype Length = Length Float
    deriving (Ord, Eq, Num, Fractional, Floating)

instance Show Length where
    show (Length a) | a < 0.001 = show (a * 1000)   ++ " mm"
                    | a < 1     = show (a * 100)    ++ " cm"
                    | a > 1000  = show (a * 0.0001) ++ " km"
                    | otherwise = show a            ++ " m"
