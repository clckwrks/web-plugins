{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, OverloadedStrings #-}
module Core where

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

data PluginsState n = PluginsState
    { pluginsPreProcs   :: Map PluginName (Text -> IO Text)
    , pluginsHandler    :: Map PluginName (Plugins n -> [Text] -> n)
    , pluginsOnShutdown :: [Cleanup]
    , pluginsRouteFn    :: Map PluginName Dynamic
    }

-- | we don't really want to give the Plugin unrestricted access to modify the PluginsState TVar. So we will use a newtype?
newtype Plugins m = Plugins { ptv :: TVar (PluginsState m) }

initPlugins :: IO (Plugins n)
initPlugins =
    do ptv <- atomically $ newTVar
              (PluginsState { pluginsPreProcs   = Map.empty
                            , pluginsHandler    = Map.empty
                            , pluginsOnShutdown = []
                            , pluginsRouteFn    = Map.empty
                            }
              )
       return (Plugins ptv)

destroyPlugins :: When -> Plugins m -> IO ()
destroyPlugins whn (Plugins ptv) =
    do pos <- atomically $ pluginsOnShutdown <$> readTVar ptv
       mapM_ (cleanup whn) pos
       return ()
    where
      cleanup whn (Cleanup whn' action)
          | isWhen whn whn' = action
          | otherwise       = return ()

-- | a bracketed combination of 'initPlugins' and 'destroyPlugins'. Takes care of passing the correct termination condition.
withPlugins :: (Plugins m -> IO a) -> IO a
withPlugins action =
    bracketOnError initPlugins
                   (destroyPlugins OnFailure)
                   (\p -> do r <- action p ; destroyPlugins OnNormal p; return r)

addPreProc :: (MonadIO m) => Plugins n -> Text -> (Text -> IO Text) -> m ()
addPreProc (Plugins tps) pname pp =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsPreProcs = Map.insert pname pp pluginsPreProcs }

getPreProcs :: (MonadIO m) => Plugins n -> m (Map Text (Text -> IO Text))
getPreProcs (Plugins tvp) =
    liftIO $ atomically $ pluginsPreProcs <$> readTVar tvp

addHandler :: (MonadIO m) => Plugins n -> Text -> (Plugins n -> [Text] -> n) -> m ()
addHandler (Plugins tps) pname ph =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsHandler = Map.insert pname ph pluginsHandler }

-- | add a new cleanup action to the top of the stack
addCleanup :: (MonadIO m) => Plugins n -> When -> IO () -> m ()
addCleanup (Plugins tps) when action =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
        ps { pluginsOnShutdown = (Cleanup when action) : pluginsOnShutdown }

addPluginRouteFn :: (MonadIO m, Typeable url) =>
                    Plugins n
                 -> Text
                 -> (url -> [(Text, Text)] -> Text)
                 -> m ()
addPluginRouteFn (Plugins tpv) pluginName routeFn =
    liftIO $ do -- putStrLn $ "Adding route for " ++ Text.unpack pluginName
                atomically $ modifyTVar' tpv $ \ps@PluginsState{..} ->
                  ps { pluginsRouteFn = Map.insert pluginName (toDyn routeFn) pluginsRouteFn }


getPluginRouteFn :: (MonadIO m, Typeable url) =>
                    Plugins n
                 -> Text
                 -> m (Maybe (url -> [(Text, Text)] -> Text))
getPluginRouteFn (Plugins ptv) pluginName =
    do -- liftIO $ putStrLn $ "looking up route function for " ++ Text.unpack pluginName
       routeFns <- liftIO $ atomically $ pluginsRouteFn <$> readTVar ptv
       case Map.lookup pluginName routeFns of
         Nothing -> do -- liftIO $ putStrLn "oops, route not found."
                       return Nothing
         (Just dyn) -> return $ fromDynamic dyn

data Plugin url n = Plugin
    { pluginName         :: PluginName
    , pluginInit         :: Plugins n -> IO (Maybe Text)
    , pluginDepends      :: [Text]   -- ^ plugins which much be initialized before this one can be
    , pluginToPathInfo   :: url -> Text
    }

initPlugin :: (Typeable url) => Plugins n -> Text -> Plugin url n -> IO ()
initPlugin plugins baseURI (Plugin{..}) =
    do -- putStrLn $ "initializing " ++ (Text.unpack pluginName)
       addPluginRouteFn plugins pluginName (\u p -> baseURI <> "/" <> pluginToPathInfo u)
       pluginInit plugins
       return ()

------------------------------------------------------------------------------
-- serve
------------------------------------------------------------------------------

serve :: Plugins n -> Text -> [Text] -> IO (Either String n)
serve plugins@(Plugins tvp) prefix path =
    do phs <- atomically $ pluginsHandler <$> readTVar tvp
       case Map.lookup prefix phs of
         Nothing  -> return $ Left  $ "Invalid plugin prefix: " ++ Text.unpack prefix
         (Just h) -> return $ Right $ (h plugins path)

------------------------------------------------------------------------------
-- MonadRoute
------------------------------------------------------------------------------

class (Monad m) => MonadRoute m url where
    askRouteFn :: m (url -> [(Text, Text)] -> Text)

mkRouteFn :: (Show url) => Text -> Text -> URLFn url
mkRouteFn baseURI prefix =
    \url params -> baseURI <> "/" <>  prefix <> "/" <> Text.pack (show url)
