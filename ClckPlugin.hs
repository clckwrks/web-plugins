{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, OverloadedStrings #-}
module ClckPlugin where

import Core
import Control.Monad       (foldM)
import Control.Monad.Trans (liftIO)
import Data.Acid
import Data.Acid.Local
import Data.Data
import qualified Data.Map as Map
import Data.SafeCopy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

------------------------------------------------------------------------------
-- ClckPlugin
------------------------------------------------------------------------------

data ClckURL
    = ViewPage
      deriving (Eq, Ord, Show, Read, Data, Typeable)

data ClckState = ClckState
    {
    }
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''ClckState)
$(makeAcidic ''ClckState [])

clckHandler :: URLFn ClckURL -> Plugins -> [Text] -> IO ()
clckHandler showRoutFn plugins paths = -- FIXME: we assume the paths decode to ViewPage for now
    do putStrLn "clck handler"
       pps <- getPreProcs plugins
       txt <- foldM (\txt pp -> pp txt) "I like cheese." (Map.elems pps)
       Text.putStrLn txt
       return ()

clckPreProcessor :: URLFn ClckURL
                 -> (Text -> IO Text)
clckPreProcessor showFnClckURL txt =
    return (Text.replace "like" "love" txt)

clckInit :: Plugins -> IO (Maybe Text)
clckInit plugins =
    do (Just clckShowFn) <- getPluginRouteFn plugins "clck"
       addPreProc plugins "clck" (clckPreProcessor clckShowFn)
       addHandler plugins "clck" (clckHandler clckShowFn)
       return Nothing

clckPlugin :: Plugin ClckURL
clckPlugin = Plugin
    { pluginName       = "clck"
    , pluginInit       = clckInit
    , pluginDepends    = []
    , pluginToPathInfo = Text.pack . show
    }

plugin :: Plugins -> Text -> IO ()
plugin plugins baseURI =
    do initPlugin plugins baseURI clckPlugin