
module Commands.About where

import           Data.Bot        (Command)
import           Network.Discord (Reply (reply))

about :: Command ()
about = do
    reply $ "**Cowbot 🤠** "
         <> "version `3.0.0` \n\n"
         <> "Git repository: <https://github.com/stereoknife/cowbot-hs> \n"
         <> "Icon by <https://twitter.com/tdtbaa/>"
