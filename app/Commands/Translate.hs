{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Commands.Translate where

import Howdy.Command ( Command, CommandInput(..), CommandWith )
import Howdy.Internal.Reaction
import Data.Language
import Control.Monad.Catch
import Control.Monad.IO.Class
import Howdy.Internal.Discord
import Data.Text (Text)
import Discord.Types
import Data.Translation
import qualified Data.Text as T
import Control.Monad.Reader
import GHC.Records
import Data.Data
import Data.Random
import Discord

transCmd :: Command
transCmd = do
    t <- ask
    trans t.args English

-- transRec :: Reaction
-- transRec = do
--     m <- ask
--     trans m.args English

-- transRecRandom :: Reaction
-- transRecRandom = do
--     m <- ask
--     l <- liftIO $ randomValue @Lang $ dataTypeOf English
--     -- reply $ langName l
--     trans m.args l

trans :: Text -> Lang -> Command
trans t l = do
    let tx = Auto t
    tr <- translate tx l
    a <- ask

    let trr = take 2 $ locTextArray tr
    if locText (last trr) == locText (head trr)
    then send "nothing to translate.."
    else writeOut a.user trr

writeOut :: User -> [LocalizedText 'KLang] -> Command
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
