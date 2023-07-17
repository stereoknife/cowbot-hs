{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Commands.Bless where
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Aeson             (FromJSON, decode)
import           Data.Random            (randomNum)
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           Discord.Types          (DiscordId (unId),
                                         Snowflake (unSnowflake), User (userId))
import           GHC.Generics           (Generic)
import           Howdy.Comptime.Command (Command, CommandInput (author))
import           Howdy.Error            (HowdyException (..), report)
import           Howdy.Internal.Discord (send)
import           Network.HTTP.Simple    (getResponseBody, httpJSON,
                                         parseRequest, setRequestIgnoreStatus,
                                         setRequestMethod, setRequestPath,
                                         setRequestQueryString)

data APIResponse = APIResponse { bookname :: Text
                               , chapter  :: Text
                               , verse    :: Text
                               , text     :: Text
                               } deriving (Generic, Show)

instance FromJSON APIResponse where

bless :: Command
bless i
    | i.author.userId.unId.unSnowflake == 182128752942120961 = do
        rand <- liftIO $ randomNum (0, 10)
        if rand /= 0 then bless' i
        else send $ fmt str
    | otherwise = bless' i
    where str = APIResponse "Streets" "1" "12" st
          st = "An MR-RL time. Me and Marc Rutzou. Me and fucking Marc Rutzou. That's fucking right, I skipped 1:13, I am a fucking legend. I've never seen a 1:13 and I never fucking will. 1:12 baby, 'til the day I fucking die. YES!! About time I get a fucking lucky break in this fucking game, motherfuckers. And I CLUTCHED the grenade launcher, that's fucking right. Yeah it's this one, right here, it's this one. My insane pace. Right here, this is the 1:12. Right here, YES! It's fucking it. I'mma have to just-- I'mma have to find the actual whole vid off of it and I'mma have to get it. DUDE, WHAT A RUSH! WHAT A RUSH! Watch this, watch when I get the grenade launcher. Look at that, see how fast my pace is? RIGHT IN THE FUCKING HEAD, YEAH. GOT A FUCKING 1:12 BABY, SEE THAT CLUTCHNESS, I AM FUCKING CLUTCH. LOOK AT THIS FUCKING LINE I TAKE, I'M LIKE, YEAH BABY, LET'S FUCKING DO THIS. I wait, I wait, I wait, right when he starts firing to try and backboost me, the double. Body armour, two quick ones, I know I'm already getting there on the perfect line. LOOK AT THE FUCKING PACE! Fifty, forty-nine, FORTY-SEVEN BABY, THAT'S FUCKING RIGHT, THAT'S FUCKING IT. Fucking PUMPED watching this one again. I waited the cinema too, 'cos I said, oh my god it might be 1:12. AND IT FUCKING IS, IT FUCKING IS BABY, YEAH. Look at me typing you guys. You guys didn't believe me, I'm typing a storm. DUDE I FUCKING JUST GOT STREETS 1:12 AND IT'S NOT COMING OFF. YES! I FUCKING DID IT, THAT'S RIGHT, I SKIPPED 1:13, I'M A LEGEND, I AM A LEGEND. I AM A FUCKING LEGEND. I am a fucking legend."


bless' :: Command
bless' i = do
    defReq <- parseRequest "https://labs.bible.org/"
    let request
            = setRequestPath "/api"
            $ setRequestMethod "GET"
            $ setRequestQueryString [ ("passage", Just $ encodeUtf8 "random")
                                    , ("type", Just $ encodeUtf8 "json")
                                    , ("formatting", Just $ encodeUtf8 "plain")
                                    ]
            $ setRequestIgnoreStatus
            defReq

    response <- httpJSON request

    let r = head $ getResponseBody @[APIResponse] response
    send $ fmt r

fmt :: APIResponse -> Text
fmt r = "**" <> bookname r <> " " <> chapter r <> ":" <> verse r <> "** " <> text r
