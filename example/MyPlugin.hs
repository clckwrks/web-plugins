{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, OverloadedStrings #-}
module MyPlugin where

import Core
import Web.Plugins.Core
import ClckPlugin
import Control.Monad.Trans (liftIO)
import Data.Acid
import Data.Acid.Local
import Data.Data
import Data.SafeCopy
import Data.Text (Text)
import qualified Data.Text as Text
import Happstack.Server
import Theme
import Web.Routes
import Web.Routes.TH

------------------------------------------------------------------------------
-- MyPlugin
------------------------------------------------------------------------------

data MyState = MyState
    {
    }
    deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''MyState)
$(makeAcidic ''MyState [])

data MyURL
    = MyUrl
    deriving (Eq, Ord, Show, Read, Data, Typeable)
$(derivePathInfo ''MyURL)
{-
data MyPluginsState = MyPluginsState
    { myAcid :: AcidState MyState
    }
-}
myPreProcessor :: URLFn ClckURL
               -> URLFn MyURL
               -> (Text -> IO Text)
myPreProcessor showFnClckURL showFnMyURL txt =
    return (Text.replace "cheese" "Haskell" txt)

{-

Things to do:

  1. open the acid-state for the plugin
  2. register a callback which uses the AcidState
  3. register an action to close the database on shutdown

-}

myInit :: ExamplePlugins -> IO (Maybe Text)
myInit plugins =
    do (Just clckShowFn) <- getPluginRouteFn plugins (pluginName clckPlugin)
       (Just myShowFn)   <- getPluginRouteFn plugins (pluginName myPlugin)
       acid <- liftIO $ openLocalState MyState
       addCleanup plugins OnNormal  (putStrLn "myPlugin: normal shutdown"  >> createCheckpointAndClose acid)
       addCleanup plugins OnFailure (putStrLn "myPlugin: failure shutdown" >> closeAcidState acid)
       addHandler plugins "my" (myPluginHandler acid clckShowFn myShowFn)
       putStrLn "myInit completed."
       return Nothing

myPluginHandler :: AcidState MyState
                -> URLFn ClckURL
                -> URLFn MyURL
                -> ExamplePlugins
                -> [Text]
                -> ServerPart Response
myPluginHandler _ _ _ _ _ =
    ok $ toResponse ("My plugin handler" :: Text)

myPlugin :: ExamplePlugin MyURL
myPlugin = Plugin
    { pluginName         = "my"
    , pluginInit         = myInit
    , pluginDepends      = ["clck"]
    , pluginToPathInfo   = toPathSegments
    , pluginPostHook     = putStrLn "my post hook."
    }

plugin :: ExamplePlugins -> Text -> IO (Maybe Text)
plugin plugins baseURI =
    do initPlugin plugins baseURI myPlugin
