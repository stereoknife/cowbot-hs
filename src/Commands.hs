{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands (commandSwitch) where

import           Control.Monad.Combinators (empty)
import           Control.Monad.Reader      (ReaderT, asks)
import           Control.Monad.State
import           Data.Aeson
import qualified Data.Aeson.Types          as JSON
import           Data.Map                  ((!?))
import qualified Data.Text                 as T
import           Discord
import qualified Discord.Requests          as R
import           Discord.Types
import           Network.HTTP.Req
import           Parser
import           Secrets                   (yt_key)
import           Translate                 (langNames, translate)
import           UnliftIO                  (MonadIO (liftIO))
import           Web.Google.Translate      (Lang (..), Target (..))

type CommandData b a = ReaderT Message (StateT T.Text DiscordHandler) a
type Command = CommandData () ()


parse :: (MonadState T.Text m, Semigroup a) => Parser a -> m (Maybe a)
parse f = state $ \t -> do
  p <- return $ runParser f t
  case p of Just (v, rest) -> (Just v, rest)
            _              -> (Nothing, "")


parse' :: (MonadState T.Text m, Monoid a) => Parser a -> m a
parse' f = do
  p <- parse f
  return $ case p of Just v -> v
                     _      -> mempty


commandSwitch :: Command
commandSwitch = do
  mt <- asks messageText
  grr <- parse alias
  return ()

permission :: Message -> Bool
permission = undefined


clap :: Command
clap = do
  ar <- parse args
  ch <- asks messageChannel
  cl <- return $ ar >>= (return . T.intercalate "👏") >>= (return . (<> "👏"))
  case cl of Just t  -> return $ restCall $ R.CreateMessage ch t
             Nothing -> empty
  pure ()


bless :: Command
bless = do
    r <- runReq defaultHttpConfig $ req
       {-Method-} GET
          {-URL-} (https "labs.bible.org" /: "api")
         {-Body-} NoReqBody
{-Response type-} jsonResponse
        {-Query-} $ "passage" =: ("random" :: T.Text)
                  <> "type" =: ("json" :: T.Text)

    pb <- return $ withObject "response" $
      \res -> do
        b <- res .: "bookname"
        c <- res .: "chapter"
        v <- res .: "verse"
        t <- res .: "text"
        return ((b, c, v, t) :: (T.Text, T.Text, T.Text, T.Text))

    rb <- return $ JSON.parse pb $ responseBody r
    ch <- asks messageChannel

    case rb of _ -> empty
               Success (b, c, v, t) -> return $ restCall $ R.CreateMessage ch $
                                        "**" <> b <> " " <> c <> ":" <> v <> "** " <> t

    pure ()


yt :: Command
yt = do
    key <- liftIO yt_key
    ch <- asks messageChannel
    mArgs <- parse rest

    wrappedId <- return $ do
      jArgs <- mArgs
      return $ do
        r <- runReq defaultHttpConfig $ req
        {-Method-} GET
            {-URL-} (https "www.googleapis.com"/:"youtube"/:"v3"/:"search")
          {-Body-} NoReqBody
  {-Response type-} jsonResponse
          {-Query-} $ "part" =: ("snippet" :: T.Text)
                      <> "maxResults" =: ("1" :: T.Text)
                      <> "key" =: key
                      <> "q" =: jArgs

        parserId <- return $ withObject "data" $
          \dat -> do
            items   <- dat    .: "items"
            item    <- return  $  head items
            id      <- item   .: "id"
            vid     <- id     .: "videoId"
            return (vid :: T.Text)

        return $ JSON.parse parserId $ responseBody r

    videoId <- case wrappedId of Just a -> liftIO a
                                 _      -> empty

    return $ case videoId of Success id -> restCall $ R.CreateMessage ch $ "https://youtube.com/watch?v=" <> id
                             _          -> restCall $ R.CreateMessage ch "Couldn't find anything 😔"

    pure ()


trans :: Command
trans = do
    mt <- parse' rest
    au <- asks messageAuthor
    ch <- asks messageChannel

    t <- liftIO $ translate mt Nothing $ Target English

    (tr, ln) <- return $ case t of Nothing -> ("", Nothing)
                                   Just v  -> v

    return $ restCall $ if (tr == mt) then R.CreateMessage ch $ "Nothing to translate.."
                        else R.CreateMessageEmbed ch T.empty $
                            let ll = case ln >>= (!?) langNames
                                     of Just l' ->  l'
                                        Nothing -> "who knever knows!"

                                fr  = EmbedField { embedFieldName = ll
                                                  , embedFieldValue = mt
                                                  , embedFieldInline = Just False
                                                  }

                                to  = EmbedField { embedFieldName = "English"
                                                  , embedFieldValue = tr
                                                  , embedFieldInline = Just False
                                                  }

                                pic = do
                                      av <- userAvatar au
                                      pure $ CreateEmbedImageUrl $
                                            "https://cdn.discordapp.com/avatars/"
                                            <> (T.pack . show $ userId au)
                                            <> "/" <> av <> ".png"

                            in def { createEmbedAuthorName = userName au
                                    , createEmbedFields     = [fr, to]
                                    , createEmbedAuthorIcon = pic
                                    }

    return ()
