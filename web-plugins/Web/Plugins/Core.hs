{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, OverloadedStrings #-}
module Web.Plugins.Core
     ( When(..)
     , Cleanup(..)
     , PluginName
     , PluginsState(..)
     , Plugins(..)
     , initPlugins
     , destroyPlugins
     , withPlugins
     , getPluginsSt
     , putPluginsSt
     , modifyPluginsSt
     , addHandler
     , addCleanup
     , addPostHook
     , getPostHooks
     , addPluginRouteFn
     , getPluginRouteFn
     , setTheme
     , getTheme
     , getConfig
     , Plugin(..)
     , initPlugin
     , serve
     ) where

import Control.Applicative    ((<$>))
import Control.Exception      (bracketOnError)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar')
import Control.Monad.Trans    (MonadIO(liftIO))
import Data.Char              (ord)
import Data.Data              (Data, Typeable)
import Data.Dynamic           (Dynamic, toDyn, fromDynamic)
import qualified Data.Text    as Text
import Data.List              (intersperse)
import Data.Map               (Map)
import qualified Data.Map     as Map
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>), mempty, mconcat)
import Data.String            (fromString)
import Data.Text              (Text)
import qualified Data.Text    as T
import Data.Text.Lazy         (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)
import Numeric                (showIntAtBase)

-- | 'When' indicates when a clean up action should be run
data When
    = Always     -- ^ always run this action when 'destroyPlugins' is called
    | OnFailure  -- ^ only run this action if 'destroyPlugins' is called with a failure present
    | OnNormal   -- ^ only run this action when 'destroyPlugins' is called with a normal shutdown
      deriving (Eq, Ord, Show)

isWhen :: When -> When -> Bool
isWhen Always _ = True
isWhen x y = x == y

data Cleanup = Cleanup When (IO ())

type PluginName = Text

data PluginsState theme n hook config st = PluginsState
    { pluginsHandler    :: Map PluginName (Plugins theme n hook config st -> [Text] -> n)
    , pluginsOnShutdown :: [Cleanup]
    , pluginsRouteFn    :: Map PluginName Dynamic
    , pluginsTheme      :: Maybe theme
    , pluginsPostHooks  :: [hook]
    , pluginsConfig     :: config
    , pluginsState      :: st
    }

-- | we don't really want to give the Plugin unrestricted access to modify the PluginsState TVar. So we will use a newtype?
newtype Plugins theme m hook config st = Plugins { ptv :: TVar (PluginsState theme m hook config st) }

-- | initialize the plugins system
initPlugins :: config -- ^ initial value for the 'config' field of 'PluginsState'
            -> st     -- ^ initial value for the 'state' field of the 'PluginsState'
            -> IO (Plugins theme n hook config st)
initPlugins config st =
    do ptv <- atomically $ newTVar
              (PluginsState { pluginsHandler    = Map.empty
                            , pluginsOnShutdown = []
                            , pluginsRouteFn    = Map.empty
                            , pluginsTheme      = Nothing
                            , pluginsPostHooks  = []
                            , pluginsConfig     = config
                            , pluginsState      = st
                            }
              )
       return (Plugins ptv)

-- | shutdown the plugins system
destroyPlugins :: When -- ^ should be 'OnFailure' or 'OnNormal'
               -> Plugins theme m hook config st -- ^ handle to the plugins
               -> IO ()
destroyPlugins whn (Plugins ptv) =
    do pos <- atomically $ pluginsOnShutdown <$> readTVar ptv
       mapM_ (cleanup whn) pos
       return ()
    where
      cleanup w (Cleanup w' action)
          | isWhen w w' = action
          | otherwise   = return ()

-- | a bracketed combination of 'initPlugins' and 'destroyPlugins'. Takes care of passing the correct termination condition.
withPlugins :: config -- ^ initial config value
            -> st     -- ^ initial state value
            -> (Plugins theme m hook config st -> IO a) -> IO a
withPlugins config st action =
    bracketOnError (initPlugins config st)
                   (destroyPlugins OnFailure)
                   (\p -> do r <- action p ; destroyPlugins OnNormal p; return r)

------------------------------------------------------------------------------
-- PluginsSt
------------------------------------------------------------------------------

-- | get the current st value from 'Plugins'
getPluginsSt :: (MonadIO m) => Plugins theme n hook config st
             -> m st
getPluginsSt (Plugins tps) =
    liftIO $ atomically $ pluginsState <$> readTVar tps

-- | put the current st value from 'Plugins'
putPluginsSt :: (MonadIO m) => Plugins theme n hook config st -> st -> m ()
putPluginsSt (Plugins tps) st =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
        ps { pluginsState = st }

-- | modify the current st value from 'Plugins'
modifyPluginsSt :: (MonadIO m) => Plugins theme n hook config st
                -> (st -> st)
                -> m ()
modifyPluginsSt (Plugins tps) f =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
        ps { pluginsState = f pluginsState }

-- | add a new route handler
addHandler :: (MonadIO m) => Plugins theme n hook config st
           -> Text -- ^ prefix which this route handles
           -> (Plugins theme n hook config st -> [Text] -> n)
           -> m ()
addHandler (Plugins tps) pname ph =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsHandler = Map.insert pname ph pluginsHandler }

-- | add a new cleanup action to the top of the stack
addCleanup :: (MonadIO m) => Plugins theme n hook config st -> When -> IO () -> m ()
addCleanup (Plugins tps) when action =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
        ps { pluginsOnShutdown = (Cleanup when action) : pluginsOnShutdown }

addPostHook :: (MonadIO m) =>
               Plugins theme n hook config st
            -> hook
            -> m ()
addPostHook (Plugins tps) postHook =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsPostHooks = postHook : pluginsPostHooks }

getPostHooks :: (MonadIO m) =>
               Plugins theme n hook config st
            -> m [hook]
getPostHooks (Plugins tps) =
    liftIO $ atomically $ pluginsPostHooks <$> readTVar tps

addPluginRouteFn :: (MonadIO m, Typeable url) =>
                    Plugins theme n hook config st
                 -> Text
                 -> (url -> [(Text, Maybe Text)] -> Text)
                 -> m ()
addPluginRouteFn (Plugins tpv) pluginName routeFn =
    liftIO $ do -- putStrLn $ "Adding route for " ++ Text.unpack pluginName
                atomically $ modifyTVar' tpv $ \ps@PluginsState{..} ->
                  ps { pluginsRouteFn = Map.insert pluginName (toDyn routeFn) pluginsRouteFn }


getPluginRouteFn :: (MonadIO m, Typeable url) =>
                    Plugins theme n hook config st
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
            Plugins theme n hook config st
         -> Maybe theme
         -> m ()
setTheme (Plugins tps) theme =
        liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsTheme = theme }

getTheme :: (MonadIO m) =>
            Plugins theme n hook config st
         -> m (Maybe theme)
getTheme (Plugins tvp) =
    liftIO $ atomically $ pluginsTheme <$> readTVar tvp

getConfig :: (MonadIO m) =>
             Plugins theme n hook config st
          -> m config
getConfig (Plugins tvp) =
    liftIO $ atomically $ pluginsConfig <$> readTVar tvp

-- | NOTE: it is possible to set the URL type incorrectly here and not get a type error. How can we fix that ?
data Plugin url theme n hook config st = Plugin
    { pluginName         :: PluginName
    , pluginInit         :: Plugins theme n hook config st -> IO (Maybe Text)
    , pluginDepends      :: [Text]   -- ^ plugins which much be initialized before this one can be
    , pluginToPathInfo   :: url -> Text
    , pluginPostHook     :: hook
    }

initPlugin :: (Typeable url) =>
              Plugins theme n hook config st
           -> Text
           -> Plugin url theme n hook config st
           -> IO (Maybe Text)
initPlugin plugins baseURI (Plugin{..}) =
    do -- putStrLn $ "initializing " ++ (Text.unpack pluginName)
       addPluginRouteFn plugins pluginName (\u p -> baseURI <> "/" <> pluginName <> pluginToPathInfo u <> paramsToQueryString (map (\(k, v) -> (k, fromMaybe mempty v)) p))
       addPostHook plugins pluginPostHook
       pluginInit plugins


paramsToQueryString :: [(Text, Text)] -> Text
paramsToQueryString [] = mempty
paramsToQueryString ps = toStrictText $ "?" <> mconcat (intersperse "&" (map paramToQueryString ps) )
    where
      toStrictText = toStrict . toLazyText

      isAlphaChar :: Char -> Bool
      isAlphaChar c    = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

      isDigitChar :: Char -> Bool
      isDigitChar c    = (c >= '0' && c <= '9')

      isOk :: Char -> Bool
      isOk c = isAlphaChar c || isDigitChar c || c `elem` ":@$-_.~"

      escapeChar c
          | c == ' '  = singleton '+'
          | isOk c    = singleton c
          | otherwise = "%" <>
                        let hexDigit n
                                | n <= 9 = head (show n)
                                | n == 10 = 'A'
                                | n == 11 = 'B'
                                | n == 12 = 'C'
                                | n == 13 = 'D'
                                | n == 14 = 'E'
                                | n == 15 = 'F'
                        in case showIntAtBase 16 hexDigit (ord c) "" of
                             []  -> "00"
                             [x] -> fromString ['0',x]
                             cs  -> fromString cs

      escapeParam :: Text -> Builder
      escapeParam p = Text.foldr (\c cs -> escapeChar c <> cs) mempty p

      paramToQueryString :: (Text, Text) -> Builder
      paramToQueryString (k,v) = (escapeParam k) <> "=" <> (escapeParam v)

------------------------------------------------------------------------------
-- serve
------------------------------------------------------------------------------

serve :: Plugins theme n hook config st
      -> Text
      -> [Text]
      -> IO (Either String n)
serve plugins@(Plugins tvp) prefix path =
    do phs <- atomically $ pluginsHandler <$> readTVar tvp
       case Map.lookup prefix phs of
         Nothing  -> return $ Left  $ "Invalid plugin prefix: " ++ Text.unpack prefix
         (Just h) -> return $ Right $ (h plugins path)
