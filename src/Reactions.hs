{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Reactions where

import           Control.Monad.Combinators (empty)
import           Control.Monad.Reader      (ReaderT, asks, lift, liftIO,
                                            runReaderT)
import qualified Data.Text                 as T
import           Discord                   (DiscordHandler, restCall)
import           Discord.Internal.Rest     (Emoji (emojiName),
                                            Message (messageAuthor, messageText),
                                            ReactionInfo (reactionChannelId, reactionEmoji, reactionMessageId))
import qualified Discord.Requests          as R
import           Web.Google.Translate      (Lang (..))

type Reaction = ReaderT ReactionInfo DiscordHandler ()

{-}
runReaction :: ReaderT r m a -> r -> m a
runReaction = runReaderT

reactionSwitch :: Reaction
reactionSwitch = do
    mid <- asks reactionMessageId
    cid <- asks reactionChannelId
    em  <- asks $ emojiName . reactionEmoji
    amt <- lift $ do
            em <- restCall $ R.GetReactions (cid, mid) em (2, R.BeforeReaction mid)
            case em of Right m -> return $ m
                       _       -> return []

    let is e = T.head e == T.head em
    if
        | length amt > 1 -> pure ()
        | is "🔣"   -> reactTranslate $ Just English
        | is "🗺"   -> reactTranslate Nothing
        | otherwise -> pure ()
    return ()

reactConvert :: Reaction
reactConvert = do
    mid <- asks reactionMessageId
    cid <- asks reactionChannelId
    msg <- lift $ do
            ms <- restCall $ R.GetChannelMessage (cid, mid)
            case ms of Right m -> return $ Just m
                       _       -> return empty

    ()
    return ()

reactTranslate :: Maybe Lang -> Reaction
reactTranslate to = do
    mid <- asks reactionMessageId
    cid <- asks reactionChannelId
    em  <- asks $ emojiName . reactionEmoji
    msg <- lift $ do
            restCall $ R.CreateReaction (cid, mid) em
            ms <- restCall $ R.GetChannelMessage (cid, mid)
            case ms of Right m -> return $ Just m
                       _       -> return empty

    transl <- case messageText <$> msg
        of Just mt -> liftIO $
                translate mt Nothing $ to
           _ -> empty

    case (transl, messageAuthor <$> msg)
        of (Trans { fromText = Just ft
                  , fromLang = Just fl
                  , toText = Just tt
                  , toLang = Just tl
                  , success = True
                  }
                , Just ma) -> do
                    lift $ restCall $ if ft == tt then R.CreateMessage cid "Nothing to translate.."
                                                  else R.CreateMessageEmbed cid T.empty $ sendEmbed ma (fl, ft) (tl, tt)
                    return ()
           _ -> return ()
-}
