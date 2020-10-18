module Types.Command where

import           Data.Text     (Text)
import           Discord       (DiscordHandler)
import           Discord.Types (Message)
import           Types.Parser  (Par)



class Par m => Command m where
    reply :: Text -> m (DiscordHandler ())
    whisper :: Text -> m (DiscordHandler ())
    react :: Text -> m (DiscordHandler ())
    embed :: Text -> m (DiscordHandler ())


