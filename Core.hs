{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar, modifyTVar')
import Control.Monad       (foldM)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Monad.State (MonadState, StateT, runStateT, get, put, modify)

import Data.Acid
import Data.Acid.Local
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Data
import Data.Dynamic
import Data.SafeCopy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Monoid
import HSP (XMLGenT, XML)

{-

Insights:

 We have two distinct phases: (ACTUALLY, THIS IS WRONG)

  - initialization that is done before we start listening for requests
  - stuff that happens inside the request handler

If we mix in stuff that is only available when we have a request into
ClckT then we have a hard time using it when there is no active
request (aka, at initialization time).

(AND THIS IS WHY)

Things are actually (probably) more difficult in the precense of hs-plugins. With
hs-plugins we potentially allow plugins to be activated while the
server is already running.

What happens if the use wants to modify parts of the global state
while in a handler? Clearly that requires STM since it is not local to
the request. Alternative, the thread could store the all the actions
that it wants do to (in writer) and they could be performed at the
end.

What are the use cases where we want a thread to be able to modify the
global state? For example, how would one-click plugin installs work?
Perhaps not all plguins can modify the global plugin state?

OBSERVATION: If the global plugin state can be manipulated from more
than one thread, then we must use STM. If the global plugin state can
only happen in one thread, then that means that the request handlers
can not manipulate the plugin state. That seems to conflict with the
goal that we have one-click installs.

-}

{-

When is a plugin in effect?

 1. when a preprocessor gets called

    - this happens in the core clckwrks code so it can't have preknowledge of the plugin

 2. when it is handling a route

 3. when so other plugin calls its methods

 4. on shutdown

 5. by a template the requires a specific plugin

-}

{-

A plugin needs to be able to:

 - generate an internal link
 - generate a link to a parent url
 - generate a page that includes internal links using the parent template function
 - register callbacks that use the plugin context (monad, url-type, etc)
 - access the 'ClckT' context

 - plugins need to initialize and free resources
 - plugin shutdown may care if this is a normal vs error shutdown

Additionally:

 - we only want to do the 'static' calculations once, not everytime we run the route

-}

import Data.Text (Text)

type URLFn url = url -> [(Text, Text)] -> Text

class ShowRoute m url where
    getRouteFn :: m (URLFn url)

data When
    = Always
    | OnFailure
    | OnNormal
      deriving (Eq, Ord, Show)

isWhen :: When -> When -> Bool
isWhen Always _ = True
isWhen x y = x == y

data Cleanup = Cleanup When (IO ())

type PluginName = Text

data PluginsState = PluginsState
    { pluginsPreProcs   :: Map PluginName (Text -> IO Text)
    , pluginsHandler    :: Map PluginName (Plugins -> [Text] -> IO ())
    , pluginsOnShutdown :: [Cleanup]
    , pluginsRouteFn    :: Map PluginName Dynamic
    }

-- | we don't really want to give the Plugin unrestricted access to modify the PluginsState TVar. So we will use a newtype?
newtype Plugins = Plugins { ptv :: TVar PluginsState }

initPlugins :: IO Plugins
initPlugins =
    do ptv <- atomically $ newTVar
              (PluginsState { pluginsPreProcs   = Map.empty
                            , pluginsHandler    = Map.empty
                            , pluginsOnShutdown = []
                            , pluginsRouteFn    = Map.empty
                            }
              )
       return (Plugins ptv)

destroyPlugins :: When -> Plugins -> IO ()
destroyPlugins whn (Plugins ptv) =
    do pos <- atomically $ pluginsOnShutdown <$> readTVar ptv
       mapM_ (cleanup whn) pos
       return ()
    where
      cleanup whn (Cleanup whn' action)
          | isWhen whn whn' = action
          | otherwise       = return ()

withPlugins :: (Plugins -> IO a) -> IO a
withPlugins action =
    bracketOnError initPlugins
                   (destroyPlugins OnFailure)
                   (\p -> do r <- action p ; destroyPlugins OnNormal p; return r)

addPreProc :: (MonadIO m) => Plugins -> Text -> (Text -> IO Text) -> m ()
addPreProc (Plugins tps) pname pp =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsPreProcs = Map.insert pname pp pluginsPreProcs }

addHandler :: (MonadIO m) => Plugins -> Text -> (Plugins -> [Text] -> IO ()) -> m ()
addHandler (Plugins tps) pname ph =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsHandler = Map.insert pname ph pluginsHandler }

-- | add a new cleanup action to the top of the stack
addCleanup :: (MonadIO m) => Plugins -> When -> IO () -> m ()
addCleanup (Plugins tps) when action =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
        ps { pluginsOnShutdown = (Cleanup when action) : pluginsOnShutdown }

addPluginRouteFn :: (MonadIO m, Typeable url) =>
                    Plugins
                 -> Text
                 -> (url -> [(Text, Text)] -> Text)
                 -> m ()
addPluginRouteFn (Plugins tpv) pluginName routeFn =
    liftIO $ atomically $ modifyTVar' tpv $ \ps@PluginsState{..} ->
              ps { pluginsRouteFn = Map.insert pluginName (toDyn routeFn) pluginsRouteFn }


getPluginRouteFn :: (MonadIO m, Typeable url) =>
                    Plugins
                 -> Text
                 -> m (Maybe (url -> [(Text, Text)] -> Text))
getPluginRouteFn (Plugins ptv) pluginName =
    do routeFns <- liftIO $ atomically $ pluginsRouteFn <$> readTVar ptv
       case Map.lookup pluginName routeFns of
         Nothing -> return Nothing
         (Just dyn) -> return $ fromDynamic dyn

data Plugin url = Plugin
    { pluginName         :: PluginName
    , pluginInit         :: Plugins -> IO (Maybe Text)
    , pluginDepends      :: [Text]   -- ^ plugins which much be initialized before this one can be
    , pluginToPathInfo   :: url -> Text
    }

initPlugin :: (Typeable url) => Plugins -> Text -> Plugin url -> IO ()
initPlugin plugins baseURI (Plugin{..}) =
    do addPluginRouteFn plugins pluginName (\u p -> baseURI <> "/" <> pluginToPathInfo u)
       pluginInit plugins
       return ()

------------------------------------------------------------------------------
-- Example
------------------------------------------------------------------------------


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
clckHandler showRoutFn (Plugins tvp) paths = -- FIXME: we assume the paths decode to ViewPage for now
    do putStrLn "clck handler"
       pps <- atomically $ pluginsPreProcs <$> readTVar tvp
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

data MyPluginsState = MyPluginsState
    { myAcid :: AcidState MyState
    }

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

myInit :: Plugins -> IO (Maybe Text)
myInit plugins =
    do (Just clckShowFn) <- getPluginRouteFn plugins "clck"
       (Just myShowFn)   <- getPluginRouteFn plugins "my"
       acid <- liftIO $ openLocalState MyState
       addCleanup plugins OnNormal  (putStrLn "myPlugin: normal shutdown"  >> createCheckpointAndClose acid)
       addCleanup plugins OnFailure (putStrLn "myPlugin: failure shutdown" >> closeAcidState acid)
       addPreProc plugins "my" (myPreProcessor clckShowFn myShowFn)
       addHandler plugins "my" (myPluginHandler acid clckShowFn myShowFn)
       putStrLn "myInit completed."
       return Nothing

myPlugin :: Plugin MyURL
myPlugin = Plugin
    { pluginName         = "my"
    , pluginInit         = myInit
    , pluginDepends      = ["clck"]
    , pluginToPathInfo   = Text.pack . show
    }

myPluginHandler _ _ _ _ _ =
    do putStrLn "My plugin handler"
       return ()

------------------------------------------------------------------------------
-- MonadRoute
------------------------------------------------------------------------------


class (Monad m) => MonadRoute m url where
    askRouteFn :: m (url -> [(Text, Text)] -> Text)

mkRouteFn :: (Show url) => Text -> Text -> URLFn url
mkRouteFn baseURI prefix =
    \url params -> baseURI <> "/" <>  prefix <> "/" <> Text.pack (show url)

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main =
    let baseURI = "http://localhost:8000"
    in
      withPlugins $ \plugins ->
          do initPlugin plugins baseURI clckPlugin
             initPlugin plugins baseURI myPlugin
             serve plugins "my" ["MyURL"]
             serve plugins "clck" ["ViewPage"]
             return ()

serve :: Plugins -> Text -> [Text] -> IO ()
serve plugins@(Plugins tvp) prefix path =
    do phs <- atomically $ pluginsHandler <$> readTVar tvp
       case Map.lookup prefix phs of
         Nothing -> putStrLn $ "Invalid plugin prefix: " ++ Text.unpack prefix
         (Just h) -> h plugins path

{-

If we want to add plugins at runtime, then we also need to be able to add routes at runtime. But, the static types would appear to make that tricky. However, by using the prefix perhaps we can know what plugin should be providing the route fn.

-}


{-
{-

The pre-processor extensions can rely on resources that only exist in the context of the plugin. For example, looking up some information in a local state and generating a link.

But that is a bit interesting, because we can have a bunch of different preprocessers, each with their own context. So, how does that work? Seems most sensible that the preprocessors all have the same, more general type, and internally they can use their `runPluginT` functions to flatten the structure?

When is a plugin in context really even used?

 - pre-processor
 - show a plugin specific page

-}
-}
{-
newtype ClckT url m a = ClckT { unClckT :: URLFn url -> m a }

instance (Functor m) => Functor (ClckT url m) where
    fmap f (ClckT fn) = ClckT $ \u -> fmap f (fn u)

instance (Monad m) => Monad (ClckT url m) where
    return a = ClckT $ const (return a)
    (ClckT m) >>= f =
        ClckT $ \u ->
            do a <- m u
               (unClckT $ f a) u

instance (Monad m) => ShowRoute (ClckT url m) url where
    getRouteFn = ClckT $ \showFn -> return showFn

data ClckURL = ClckURL
-}

{-

data Plugin url st  = Plugin
    { pluginInit       :: StateT PluginsState IO st
    }

--    , pluginPreProcess ::
--    , pluginRoute    :: url -> [(Text, Text)] -> Text
--    , pluginTemplate :: XMLGenT m XML
--    , pluginRegister :: ClckT ClckURL (ServerPartT IO) (m Response) -- ??


addPreProc :: (MonadState PluginsState m, MonadIO m) => Text -> (Text -> ClckT ClckURL IO Text) -> m ()
addPreProc pname pp =
    modify $ \clckSt@PluginsState{..} ->
        clckSt { clckPreProcs = Map.insert pname pp clckPreProcs }

-- | add a new cleanup action to the top of the stack
addCleanup :: When -> IO () -> StateT PluginsState IO ()
addCleanup when action =
    modify $ \clckSt@PluginsState{..} ->
      clckSt { clckOnShutdown = (Cleanup when action) : clckOnShutdown }

{-

Initializing a plugin generally has side effects. For example, it adds
additional preprocessors to 'PluginsState'. But it can also do things
like open a database, and may need to register finalization actions.

-}
initPlugin :: Plugin url st -> StateT PluginsState IO st
initPlugin Plugin{..} =
    pluginInit
-}