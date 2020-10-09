{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
module ClckPlugin where

import Core
import Web.Plugins.Core
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
import HSP
import Web.Routes
import Web.Routes.TH
import Theme
import Language.Haskell.HSX.QQ

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
            -> ExamplePlugins
            -> [Text]
            -> ServerPart Response
clckHandler showRoutFn plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e) -> notFound $ toResponse (show e)
      (Right ViewPage) ->
          themeTemplate plugins "cheese." () [hsx| <p>plugins are cool.</p> |]

clckInit :: ExamplePlugins -> IO (Maybe Text)
clckInit plugins =
    do (Just clckShowFn) <- getPluginRouteFn plugins "clck"
       addHandler plugins "clck" (clckHandler clckShowFn)
       return Nothing

clckPlugin :: ExamplePlugin ClckURL
clckPlugin = Plugin
    { pluginName       = "clck"
    , pluginInit       = clckInit
    , pluginDepends    = []
    , pluginToPathInfo = toPathSegments
    , pluginPostHook   = putStrLn "clck post hook."
    }

plugin :: ExamplePlugins -> Text -> IO (Maybe Text)
plugin plugins baseURI =
    do initPlugin plugins baseURI clckPlugin
