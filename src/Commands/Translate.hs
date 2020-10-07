{-# LANGUAGE OverloadedStrings #-}

module Commands.Translate ( comTranslate
                          ) where

import           Commands.Base        (Command, parse')
import           Control.Monad.Reader (MonadIO (liftIO), asks)
import qualified Data.Text            as T
import           Discord              (restCall)
import qualified Discord.Requests     as R
import           Discord.Types        (Message (messageAuthor, messageChannel))
import           Parser               (rest)
import           Translate            (Trans (Trans, fromLang, fromText, success, toLang, toText),
                                       sendEmbed, translate)
import           Web.Google.Translate (Lang (..))

comTranslate :: Command ()
comTranslate = do
    return $ print "transin"
    mt <- parse' rest
    au <- asks messageAuthor
    ch <- asks messageChannel

    transl <- liftIO $ translate mt Nothing $ Just English

    case transl
        of Trans { fromText = Just ft
                  , fromLang = Just fl
                  , toText = Just tt
                  , toLang = Just tl
                  , success = True
                  } -> do
                    liftIO $ return $ restCall $ if ft == tt then R.CreateMessage ch "Nothing to translate.."
                                                             else R.CreateMessageEmbed ch T.empty $ sendEmbed au (fl, ft) (tl, tt)
                    return ()
           _ -> return ()

    return ()
