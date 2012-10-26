{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, OverloadedStrings #-}
module MyPlugin where

import Core
import ClckPlugin
import Control.Monad.Trans (liftIO)
import Data.Acid
import Data.Acid.Local
import Data.Data
import Data.SafeCopy
import Data.Text (Text)
import qualified Data.Text as Text
import Happstack.Server

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
    = MyURL
    deriving (Eq, Ord, Show, Read, Data, Typeable)
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

myInit :: Plugins (ServerPart Response) -> IO (Maybe Text)
myInit plugins =
    do (Just clckShowFn) <- getPluginRouteFn plugins "clck"
       (Just myShowFn)   <- getPluginRouteFn plugins "my"
       acid <- liftIO $ openLocalState MyState
       addCleanup plugins OnNormal  ({- putStrLn "myPlugin: normal shutdown"  >> -} createCheckpointAndClose acid)
       addCleanup plugins OnFailure ({- putStrLn "myPlugin: failure shutdown" >> -} closeAcidState acid)
       addPreProc plugins "my" (myPreProcessor clckShowFn myShowFn)
       addHandler plugins "my" (myPluginHandler acid clckShowFn myShowFn)
       -- putStrLn "myInit completed."
       return Nothing

myPluginHandler :: AcidState MyState
                -> URLFn ClckURL
                -> URLFn MyURL
                -> Plugins (ServerPart Response)
                -> [Text]
                -> ServerPart Response
myPluginHandler _ _ _ _ _ =
    ok $ toResponse ("My plugin handler" :: Text)

myPlugin :: Plugin MyURL (ServerPart Response)
myPlugin = Plugin
    { pluginName         = "my"
    , pluginInit         = myInit
    , pluginDepends      = ["clck"]
    , pluginToPathInfo   = Text.pack . show
    }


plugin :: Plugins (ServerPart Response) -> Text -> IO ()
plugin plugins baseURI =
    do initPlugin plugins baseURI myPlugin
