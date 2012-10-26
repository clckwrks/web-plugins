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
import Happstack.Server
import Web.Routes
import Web.Routes.TH

------------------------------------------------------------------------------
-- ClckPlugin
------------------------------------------------------------------------------

data ClckURL
    = ViewPage
      deriving (Eq, Ord, Show, Read, Data, Typeable)
$(derivePathInfo ''ClckURL)

data ClckState = ClckState
    {
    }
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''ClckState)
$(makeAcidic ''ClckState [])

clckHandler :: URLFn ClckURL
            -> Plugins (ServerPart Response)
            -> [Text]
            -> ServerPart Response
clckHandler showRoutFn plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e) -> notFound $ toResponse (show e)
      (Right ViewPage) ->
          do pps <- liftIO $ getPreProcs plugins
             txt <- liftIO $ foldM (\txt pp -> pp txt) "I like cheese." (Map.elems pps)
             ok $ toResponse txt

clckPreProcessor :: URLFn ClckURL
                 -> (Text -> IO Text)
clckPreProcessor showFnClckURL txt =
    return (Text.replace "like" "love" txt)

clckInit :: Plugins (ServerPart Response) -> IO (Maybe Text)
clckInit plugins =
    do (Just clckShowFn) <- getPluginRouteFn plugins "clck"
       addPreProc plugins "clck" (clckPreProcessor clckShowFn)
       addHandler plugins "clck" (clckHandler clckShowFn)
       return Nothing

clckPlugin :: Plugin ClckURL (ServerPart Response)
clckPlugin = Plugin
    { pluginName       = "clck"
    , pluginInit       = clckInit
    , pluginDepends    = []
    , pluginToPathInfo = Text.pack . show
    }

plugin :: Plugins (ServerPart Response) -> Text -> IO ()
plugin plugins baseURI =
    do initPlugin plugins baseURI clckPlugin