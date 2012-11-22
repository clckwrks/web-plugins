{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, OverloadedStrings #-}
module Web.Plugin.Core where

import Control.Applicative
import Control.Exception
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar')
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Data
import Data.Dynamic
import qualified Data.Text as Text
import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import System.Plugins.Load

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

data PluginsState theme n hook config ppm = PluginsState
    { pluginsPreProcs   :: Map PluginName (Text -> ppm Text)
    , pluginsHandler    :: Map PluginName (Plugins theme n hook config ppm -> [Text] -> n)
    , pluginsOnShutdown :: [Cleanup]
    , pluginsRouteFn    :: Map PluginName Dynamic
    , pluginsTheme      :: Maybe theme
    , pluginsPostHooks  :: [hook]
    , pluginsConfig     :: config
    }

-- | we don't really want to give the Plugin unrestricted access to modify the PluginsState TVar. So we will use a newtype?
newtype Plugins theme m hook config ppm = Plugins { ptv :: TVar (PluginsState theme m hook config ppm) }

initPlugins :: config -> IO (Plugins theme n hook config ppm)
initPlugins config =
    do ptv <- atomically $ newTVar
              (PluginsState { pluginsPreProcs   = Map.empty
                            , pluginsHandler    = Map.empty
                            , pluginsOnShutdown = []
                            , pluginsRouteFn    = Map.empty
                            , pluginsTheme      = Nothing
                            , pluginsPostHooks  = []
                            , pluginsConfig     = config
                            }
              )
       return (Plugins ptv)

destroyPlugins :: When -> Plugins theme m hook config ppm -> IO ()
destroyPlugins whn (Plugins ptv) =
    do pos <- atomically $ pluginsOnShutdown <$> readTVar ptv
       mapM_ (cleanup whn) pos
       return ()
    where
      cleanup w (Cleanup w' action)
          | isWhen w w' = action
          | otherwise   = return ()

-- | a bracketed combination of 'initPlugins' and 'destroyPlugins'. Takes care of passing the correct termination condition.
withPlugins :: config -> (Plugins theme m hook config ppm -> IO a) -> IO a
withPlugins config action =
    bracketOnError (initPlugins config)
                   (destroyPlugins OnFailure)
                   (\p -> do r <- action p ; destroyPlugins OnNormal p; return r)

addPreProc :: (MonadIO m) => Plugins theme n hook config ppm
           -> Text
           -> (Text -> ppm Text)
           -> m ()
addPreProc (Plugins tps) pname pp =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsPreProcs = Map.insert pname pp pluginsPreProcs }

getPreProcs :: (MonadIO m) => Plugins theme n hook config ppm -> m (Map Text (Text -> ppm Text))
getPreProcs (Plugins tvp) =
    liftIO $ atomically $ pluginsPreProcs <$> readTVar tvp

addHandler :: (MonadIO m) => Plugins theme n hook config ppm -> Text -> (Plugins theme n hook config ppm -> [Text] -> n) -> m ()
addHandler (Plugins tps) pname ph =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsHandler = Map.insert pname ph pluginsHandler }

-- | add a new cleanup action to the top of the stack
addCleanup :: (MonadIO m) => Plugins theme n hook config ppm -> When -> IO () -> m ()
addCleanup (Plugins tps) when action =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
        ps { pluginsOnShutdown = (Cleanup when action) : pluginsOnShutdown }

addPostHook :: (MonadIO m) =>
               Plugins theme n hook config ppm
            -> hook
            -> m ()
addPostHook (Plugins tps) postHook =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsPostHooks = postHook : pluginsPostHooks }

getPostHooks :: (MonadIO m) =>
               Plugins theme n hook config ppm
            -> m [hook]
getPostHooks (Plugins tps) =
    liftIO $ atomically $ pluginsPostHooks <$> readTVar tps

addPluginRouteFn :: (MonadIO m, Typeable url) =>
                    Plugins theme n hook config ppm
                 -> Text
                 -> (url -> [(Text, Maybe Text)] -> Text)
                 -> m ()
addPluginRouteFn (Plugins tpv) pluginName routeFn =
    liftIO $ do -- putStrLn $ "Adding route for " ++ Text.unpack pluginName
                atomically $ modifyTVar' tpv $ \ps@PluginsState{..} ->
                  ps { pluginsRouteFn = Map.insert pluginName (toDyn routeFn) pluginsRouteFn }


getPluginRouteFn :: (MonadIO m, Typeable url) =>
                    Plugins theme n hook config ppm
                 -> Text
                 -> m (Maybe (url -> [(Text, Maybe Text)] -> Text))
getPluginRouteFn (Plugins ptv) pluginName =
    do -- liftIO $ putStrLn $ "looking up route function for " ++ Text.unpack pluginName
       routeFns <- liftIO $ atomically $ pluginsRouteFn <$> readTVar ptv
       case Map.lookup pluginName routeFns of
         Nothing -> do -- liftIO $ putStrLn "oops, route not found."
                       return Nothing
         (Just dyn) -> return $ fromDynamic dyn

setTheme :: (MonadIO m) =>
            Plugins theme n hook config ppm
         -> Maybe theme
         -> m ()
setTheme (Plugins tps) theme =
        liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsTheme = theme }

getTheme :: (MonadIO m) =>
            Plugins theme n hook config ppm
         -> m (Maybe theme)
getTheme (Plugins tvp) =
    liftIO $ atomically $ pluginsTheme <$> readTVar tvp

getConfig :: (MonadIO m) =>
             Plugins theme n hook config ppm
          -> m config
getConfig (Plugins tvp) =
    liftIO $ atomically $ pluginsConfig <$> readTVar tvp

-- | NOTE: it is possible to set the URL type incorrectly here and not get a type error. How can we fix that ?
data Plugin url theme n hook config ppm = Plugin
    { pluginName         :: PluginName
    , pluginInit         :: Plugins theme n hook config ppm -> IO (Maybe Text)
    , pluginDepends      :: [Text]   -- ^ plugins which much be initialized before this one can be
    , pluginToPathInfo   :: url -> Text
    , pluginPostHook     :: hook
    }

initPlugin :: (Typeable url) =>
              Plugins theme n hook config ppm
           -> Text
           -> Plugin url theme n hook config ppm
           -> IO (Maybe Text)
initPlugin plugins baseURI (Plugin{..}) =
    do -- putStrLn $ "initializing " ++ (Text.unpack pluginName)
       addPluginRouteFn plugins pluginName (\u p -> baseURI <> "/" <> pluginName <> pluginToPathInfo u)
       addPostHook plugins pluginPostHook
       pluginInit plugins

------------------------------------------------------------------------------
-- serve
------------------------------------------------------------------------------

serve :: Plugins theme n hook config ppm -> Text -> [Text] -> IO (Either String n)
serve plugins@(Plugins tvp) prefix path =
    do phs <- atomically $ pluginsHandler <$> readTVar tvp
       case Map.lookup prefix phs of
         Nothing  -> return $ Left  $ "Invalid plugin prefix: " ++ Text.unpack prefix
         (Just h) -> return $ Right $ (h plugins path)

loadPlugin :: Plugins theme a hook config ppm
           -> Text        -- ^ baseURI
           -> FilePath    -- ^ object file .hi
           -> [FilePath]  -- ^ include paths
           -> IO (Maybe Text)
loadPlugin plugins baseURI obj incs =
    do status <- load_ obj incs "plugin"
       case status of
         (LoadFailure errs) -> return $ Just $ Text.pack $ unlines errs
         (LoadSuccess _module plugin) ->
             do plugin plugins baseURI

loadPlugin_ :: Plugins theme a hook config ppm
           -> Text        -- ^ baseURI
           -> FilePath    -- ^ object file .hi
           -> [FilePath]  -- ^ include paths
           -> IO ()
loadPlugin_ plugins baseURI obj incs =
    do me <- loadPlugin plugins baseURI obj incs
       case me of
         Nothing -> return ()
         (Just e) -> error $ Text.unpack e

loadTheme :: Plugins theme a hook config ppm
          -> FilePath
          -> [FilePath]
          -> IO ()
loadTheme plugins themeObj incs =
    do status <- load_ themeObj incs "theme"
       case status of
         (LoadFailure errs) ->
             error $ unlines errs
         (LoadSuccess _module theme) ->
             setTheme plugins (Just theme)
