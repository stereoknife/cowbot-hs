{-# LANGUAGE OverloadedStrings #-}

module Commands.Bless (bless) where

import           Commands.Base             (Command, send)
import           Control.Monad.Combinators (empty)
import           Control.Monad.Reader      (asks)
import           Data.Aeson                (Result (Success), withObject, (.:))
import qualified Data.Aeson.Types          as JSON
import qualified Data.Text                 as T
import           Discord                   (restCall)
import qualified Discord.Requests          as R
import           Discord.Types             (Message (messageChannel))
import           Network.HTTP.Req          (GET (GET), NoReqBody (NoReqBody),
                                            defaultHttpConfig, https,
                                            jsonResponse, req, responseBody,
                                            runReq, (/:), (=:))

import           Debug.Trace               (trace)

bless :: Command
bless = do
    return $ trace "blessin"
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
               Success (b, c, v, t) -> send $ restCall $ R.CreateMessage ch $
                                        "**" <> b <> " " <> c <> ":" <> v <> "** " <> t

    pure ()
