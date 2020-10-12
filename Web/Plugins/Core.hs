{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, OverloadedStrings #-}
{- | @web-plugins@ is a very general purpose plugin system for web applications.

It provides facilities for loading multiple plugins and a single
theme. In the future, the @web-plugins-dynamic@ library will allow
plugins and themes to be loaded and unloaded at runtime.

A key aspect of @web-plugins@ is that all plugins for a particular system
have the same type signature. This is what makes it possible to load
new plugins at runtime.

This plugin system is not tied to any particular web server framework
or template engine.

There are four steps to using @web-plugins@:

 1. initialize the plugins system

 2. initialize the individual plugins

 3. set the theme

 4. route incoming requests to the correct plugin

To use @web-plugins@, you first initialize a 'Plugins' handle.

The 'Plugins' handle is heavily parameterized:

> newtype Plugins theme m hook config st = ...

[@theme@] is (not suprisingly) the type for you theme.

[@m@] is the monad that your plugin handlers will run in. (e.g., @ServerPart@)

[@hook@] is additional actions that should be called after the plugins have been initialized

[@config@] provides read-only configuration information

[@st@] provides mutable state that is shared between all plugins. (There is a separate mechanism for plugin-local state.)

The plugin system is typically started by using 'withPlugins'. Though,
if needed, you can call 'initPlugins' and 'destroyPlugins' instead.

The 'Plugin' record is used to create a plugin:

@
data Plugin url theme n hook config st = Plugin
    { pluginName           :: PluginName
    , pluginInit           :: Plugins theme n hook config st -> IO (Maybe Text)
    , pluginDepends        :: [PluginName]   -- ^ plugins which much be initialized before this one can be
    , pluginToPathSegments :: url -> Text
    , pluginPostHook       :: hook
    }
@

You will note that it has the same type parameters as 'Plugins' plus an additional 'url' parameter.

[@pluginName@] is a simple 'Text' value which should uniquely identify the plugin.

[@pluginInit@] will be called automatically when the plugin is loaded.

[@pluginDepends@] is a list of plugins which must be loaded before this plugin can be initialized.

[@pluginToPathSegments@] is the function that is used to convert the 'url' type to the URL path segments

[@pluginPostHook@] is the hook that you want called after the system has been initialized.

A 'Plugin' is initialized using the 'initPlugin' function (which calls the 'pluginInit' field among other things).

@
-- | initialize a plugin
initPlugin :: (Typeable url) =>
              Plugins theme n hook config st    -- ^ 'Plugins' handle
           -> Text                              -- ^ base URI to prepend to generated URLs
           -> Plugin url theme n hook config st -- ^ 'Plugin' to initialize
           -> IO (Maybe Text)                   -- ^ possible error message
@

A lot of the magic happens in the 'pluginInit' function in the
'Plugin' record. Let's look at a simple example. We will use the
following type aliases to parameterize the 'Plugins' and 'Plugin'
type:

@
type ExamplePlugins    = Plugins    Theme (ServerPart Response) (IO ()) () ()
type ExamplePlugin url = Plugin url Theme (ServerPart Response) (IO ()) () ()
@

Here is the initialization function for 'myPlugin':

@
myInit :: ExamplePlugins -> IO (Maybe Text)
myInit plugins =
    do (Just clckShowFn) <- getPluginRouteFn plugins (pluginName clckPlugin)
       (Just myShowFn)   <- getPluginRouteFn plugins (pluginName myPlugin)
       acid <- liftIO $ openLocalState MyState
       addCleanup plugins OnNormal  (putStrLn "myPlugin: normal shutdown"  >> createCheckpointAndClose acid)
       addCleanup plugins OnFailure (putStrLn "myPlugin: failure shutdown" >> closeAcidState acid)
       addHandler plugins (pluginName myPlugin) (myPluginHandler acid clckShowFn myShowFn)
       putStrLn "myInit completed."
       return Nothing
@

There are a few things to note here:

'getPluginRouteFn' is used to retrieve the the URL route showing
function for various plugins. In this case, the plugin needs to
generate routes for itself and also routes in the 'clckPlugin'.

Next it opens up an 'AcidState'. It then registers two different
cleanup functions. The 'OnNormal' cleanup will only be called if the
system is shutdown normally. The 'OnFailure' will be called if the
system is shutdown due to some error condition. If we wanted to
perform the same shutdown procedure regardless of termination cause,
we could use the 'Always' condition instead.

the 'addHandler' then registers the function which route requests for
this plugin:

@
addHandler :: MonadIO m =>
              Plugins theme n hook config st
            -> PluginName -- plugin name / prefix
            -> (Plugins theme n hook config st -> [Text] -> n)
            -> m ()
@

Each plugin should be registered using a unique prefix. When
the handler is called it will be passed the 'Plugins' handle and a
list of 'Text' values. In practice, the list 'Text' values is
typically the unconsumed path segments from the URL.

Setting the theme is done by calling the 'setTheme' function:

@
-- | set the current 'theme'
setTheme :: (MonadIO m) =>
            Plugins theme n hook config st
         -> Maybe theme
         -> m ()
@

Setting the theme to 'Nothing' will unload the theme but not load a new one.

Incoming requests are routed to the various plugins via the 'serve' function:

@
-- | serve requests using the 'Plugins' handle
serve :: Plugins theme n hook config st -- ^ 'Plugins' handle
      -> PluginName -- ^ name of the plugin to handle this request
      -> [Text]     -- ^ unconsume path segments to pass to handler
      -> IO (Either String n)
@

The expected usage is that you are going to have request with a url such as:

> /my/extra/path/segments

The code will treat the first path segment as the plugin to be called and pass in the remaining segments as the @[Text]@ arguments:

> serve plugins "my" ["extra","path","segments"]

the 'serve' function itself knows nothing about the web -- it is
framework agnostic. Here is a simple @main@ function that shows how to
tie everything together:

> main :: IO ()
> main =
>   withPlugins () () $ \plugins ->
>     do initPlugin plugins "" clckPlugin
>        initPlugin plugins "" myPlugin
>        setTheme plugins (Just theme)
>        hooks <- getPostHooks plugins
>        sequence_ hooks
>        simpleHTTP nullConf $ path $ \p -> do
>          ps <- fmap rqPaths askRq
>          r <- liftIO $ serve plugins p (map Text.pack ps)
>          case r of
>            (Left e) -> internalServerError $ toResponse e
>            (Right sp) -> sp

In this example, we do not use the @config@ or @st@ parameters so we just set them to @()@.

Note that we are responsible for calling the hooks after we have initialized all the plugins.

-}
module Web.Plugins.Core
     ( When(..)
     , Cleanup(..)
     , PluginName
     , PluginsState(..)
     , Plugins(..)
     , Rewrite(..)
     , RewriteIncoming
     , RewriteOutgoing
     , initPlugins
     , destroyPlugins
     , withPlugins
     , getPluginsSt
     , putPluginsSt
     , addPluginState
     , getPluginState
     , modifyPluginsSt
     , addHandler
     , addCleanup
     , addPostHook
     , getPostHooks
     , addPluginRouteFn
     , getPluginRouteFn
     , getRewriteFn
     , setRewriteFn
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
import Data.Binary.Builder    (toLazyByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Char              (ord)
import Data.Data              (Data, Typeable)
import Data.Dynamic           (Dynamic, toDyn, fromDynamic)
import qualified Data.Text    as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.List              (intersperse)
import Data.Map               (Map)
import qualified Data.Map     as Map
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>), mempty, mconcat)
import Data.String            (fromString)
import Data.Text              (Text)
import qualified Data.Text    as T
import Data.Text.Encoding     (decodeUtf8)
import Data.Text.Lazy         (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)
import Network.HTTP.Types     (encodePathSegments)
import Numeric                (showIntAtBase)

-- | 'When' indicates when a clean up action should be run
data When
    = Always     -- ^ always run this action when 'destroyPlugins' is called
    | OnFailure  -- ^ only run this action if 'destroyPlugins' is called with a failure present
    | OnNormal   -- ^ only run this action when 'destroyPlugins' is called with a normal shutdown
      deriving (Eq, Ord, Show)

isWhen :: When -> When -> Bool
isWhen _ Always = True
isWhen x y = x == y

-- | A 'Cleanup' is an 'IO' action to run when the server shuts
-- down. The server can either shutdown normally or due to a
-- failure. The 'When' parameter indicates when an action should run.
data Cleanup = Cleanup When (IO ())

-- | The 'PluginName' should uniquely identify a plugin -- though we
-- currently have no way to enforce that.
type PluginName = Text

-- | Rewrite or Redirect
data Rewrite
  = Rewrite  -- ^ rewrite the URL internally -- does not affect the URL displayed to the user
  | Redirect (Maybe Text) -- ^ perform a 303 redirect to a different URL
  deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | rewrite the URL from a Request before routing it
type RewriteIncoming = IO ([Text] -> [(Text, Maybe Text)] -> Maybe (Rewrite, [Text], [(Text, Maybe Text)]))

-- | rewrite a URL that is going to end up in a HTML document or other output
type RewriteOutgoing = IO ([Text] -> [(Text, Maybe Text)] -> Maybe ([Text], [(Text, Maybe Text)]))

-- | The 'PluginsState' record holds all the record keeping
-- information needed for loading, unloading, and invoking plugins. In
-- theory you should not be modifying or inspecting this structure
-- directly -- only calling the helper functions that modify or read
-- it.
data PluginsState theme n hook config st = PluginsState
    { pluginsHandler     :: Map PluginName (Plugins theme n hook config st -> [Text] -> n)
    , pluginsOnShutdown  :: [Cleanup]
    , pluginsRouteFn     :: Map PluginName (Text, Dynamic) -- ^ baseURI, url -> [Text]
    , pluginsPluginState :: Map PluginName (TVar Dynamic)  -- ^ per-plugin state
    , pluginsTheme       :: Maybe theme
    , pluginsPostHooks   :: [hook]
    , pluginsConfig      :: config
    , pluginsState       :: st
    , pluginsRewrite     :: Maybe (RewriteIncoming, RewriteOutgoing) -- ^ functions rewrite the incoming and outgoing URLs
    }

-- | The 'Plugins' type is the handle to the plugins system. Generally
-- you will have exactly one 'Plugins' value in your app.
--
-- see also 'withPlugins'
newtype Plugins theme m hook config st = Plugins { ptv :: TVar (PluginsState theme m hook config st) }

-- | initialize the plugins system
--
-- see also 'withPlugins'
initPlugins :: config -- ^ initial value for the 'config' field of 'PluginsState'
            -> st     -- ^ initial value for the 'state' field of the 'PluginsState'
            -> IO (Plugins theme n hook config st)
initPlugins config st =
    do ptv <- atomically $ newTVar
              (PluginsState { pluginsHandler     = Map.empty
                            , pluginsOnShutdown  = []
                            , pluginsRouteFn     = Map.empty
                            , pluginsPluginState = Map.empty
                            , pluginsTheme       = Nothing
                            , pluginsPostHooks   = []
                            , pluginsConfig      = config
                            , pluginsState       = st
                            , pluginsRewrite     = Nothing
                            }
              )
       return (Plugins ptv)

-- | shutdown the plugins system
--
-- see also 'withPlugins'
destroyPlugins :: When                           -- ^ should be 'OnFailure' or 'OnNormal'
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

-- | get the current @st@ value from 'Plugins'
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
addHandler :: (MonadIO m) =>
              Plugins theme n hook config st
           -> PluginName -- ^ prefix which this route handles
           -> (Plugins theme n hook config st -> [Text] -> n)
           -> m ()
addHandler (Plugins tps) pname ph =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsHandler = Map.insert pname ph pluginsHandler }

-- | add a new plugin-local state
addPluginState :: (MonadIO m, Typeable state) => Plugins theme n hook config st
               -> PluginName -- plugin name
               -> state
               -> m ()
addPluginState (Plugins tps) pname state =
    liftIO $ atomically $
           do stateTV <- newTVar (toDyn state)
              modifyTVar' tps $ \ps@PluginsState{..} ->
                    ps { pluginsPluginState = Map.insert pname stateTV pluginsPluginState }

-- | Get the state for a particular plugin
--
-- per-plugin state is optional. This will return 'Nothing' if the
-- plugin did not register any local state.
getPluginState :: (MonadIO m, Typeable state) =>
                  Plugins theme n hook config st
               -> Text -- plugin name
               -> m (Maybe state)
getPluginState (Plugins ptv) pluginName =
    do states <- liftIO $ atomically $ pluginsPluginState <$> readTVar ptv
       case Map.lookup pluginName states of
         Nothing -> return Nothing
         (Just tvar) ->
             do dyn <- liftIO $ atomically $ readTVar tvar
                return $ fromDynamic dyn

-- | add a new cleanup action to the top of the stack
addCleanup :: (MonadIO m) => Plugins theme n hook config st -> When -> IO () -> m ()
addCleanup (Plugins tps) when action =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
        ps { pluginsOnShutdown = (Cleanup when action) : pluginsOnShutdown }

-- | add a new post initialization hook
addPostHook :: (MonadIO m) =>
               Plugins theme n hook config st
            -> hook
            -> m ()
addPostHook (Plugins tps) postHook =
    liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsPostHooks = postHook : pluginsPostHooks }

-- | get all the post initialization hooks
getPostHooks :: (MonadIO m) =>
               Plugins theme n hook config st
            -> m [hook]
getPostHooks (Plugins tps) =
    liftIO $ atomically $ pluginsPostHooks <$> readTVar tps

-- | add the routing function for a plugin
--
-- see also: 'getPluginRouteFn'
addPluginRouteFn :: (MonadIO m, Typeable url) =>
                    Plugins theme n hook config st
                 -> PluginName
                 -> Text -- ^ baseURI
                 -> (url -> [Text]) -- ^ url to path segments
                 -> m ()
addPluginRouteFn (Plugins tpv) pluginName baseURI routeFn =
    liftIO $ do -- putStrLn $ "Adding route for " ++ Text.unpack pluginName
                atomically $ modifyTVar' tpv $ \ps@PluginsState{..} ->
                  ps { pluginsRouteFn = Map.insert pluginName (baseURI, (toDyn routeFn)) pluginsRouteFn }

-- | get the plugin routing function for the named plugin
--
-- see also: 'addPluginRouteFn'
getPluginRouteFn :: (MonadIO m, Typeable url) =>
                    Plugins theme n hook config st
                 -> PluginName -- ^ name of plugin
                 -> m (Maybe (url -> [(Text, Maybe Text)] -> Text))
getPluginRouteFn (Plugins ptv) pluginName =
    do -- liftIO $ putStrLn $ "looking up route function for " ++ Text.unpack pluginName
       ps <- liftIO $ atomically $ readTVar ptv
       -- check if there is a plugin with this prefix
       case Map.lookup pluginName (pluginsRouteFn ps) of
         Nothing -> do -- liftIO $ putStrLn "oops, route not found."
                       pure Nothing
         (Just (baseURI, dyn)) ->
           -- extract the URL show function for this plugin
           case fromDynamic dyn of
             Nothing       -> pure Nothing
             (Just showFn) ->
               -- get rewrite function
               do f <- case pluginsRewrite ps of
                    Nothing -> pure $ \pathSegments params -> Nothing
                    (Just (_, outgoingFn)) ->
                      do f <- liftIO outgoingFn
                         pure $ f
                  pure $ Just $ \u p ->
                    let pathSegments = pluginName : (showFn u)
                    in let (paths, params) =
                             case f pathSegments p of
                               Nothing -> (pathSegments, p)
                               (Just (pathSegments', p')) -> (pathSegments', p')
                       in baseURI <> (decodeUtf8 $ BS.toStrict $ toLazyByteString $ encodePathSegments pathSegments)  <> paramsToQueryString (map (\(k, v) -> (k, fromMaybe mempty v)) params)

-- | set the current @theme@
setTheme :: (MonadIO m) =>
            Plugins theme n hook config st
         -> Maybe theme
         -> m ()
setTheme (Plugins tps) theme =
        liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
              ps { pluginsTheme = theme }

-- | get the current @theme@
getTheme :: (MonadIO m) =>
            Plugins theme n hook config st
         -> m (Maybe theme)
getTheme (Plugins tvp) =
    liftIO $ atomically $ pluginsTheme <$> readTVar tvp

-- | get the @config@ value from the 'Plugins' type
getConfig :: (MonadIO m) =>
             Plugins theme n hook config st
          -> m config
getConfig (Plugins tvp) =
    liftIO $ atomically $ pluginsConfig <$> readTVar tvp

setRewriteFn :: (MonadIO m) =>
                Plugins theme n hook config st
             -> Maybe (RewriteIncoming, RewriteOutgoing)
             -> m ()
setRewriteFn (Plugins tps) f =
  liftIO $ atomically $ modifyTVar' tps $ \ps@PluginsState{..} ->
    ps { pluginsRewrite = f }

getRewriteFn :: (MonadIO m) =>
                Plugins theme n hook config st
             -> m (Maybe (RewriteIncoming, RewriteOutgoing))
getRewriteFn (Plugins tps) =
  liftIO $ atomically $ fmap pluginsRewrite $ readTVar tps

-- | NOTE: it is possible to set the URL type incorrectly here and not get a type error. How can we fix that ?
data Plugin url theme n hook config st = Plugin
    { pluginName           :: PluginName
    , pluginInit           :: Plugins theme n hook config st -> IO (Maybe Text)
    , pluginDepends        :: [PluginName]  -- ^ plugins which much be initialized before this one can be
    , pluginToPathSegments :: url -> [Text] -- ^ convert url to URL path segments
    , pluginPostHook       :: hook
    }

-- | initialize a plugin
initPlugin :: (Typeable url) =>
              Plugins theme n hook config st    -- ^ 'Plugins' handle
           -> Text                              -- ^ base URI to prepend to generated URLs
           -> Plugin url theme n hook config st -- ^ 'Plugin' to initialize
           -> IO (Maybe Text)                   -- ^ possible error message
initPlugin plugins baseURI (Plugin{..}) =
    do -- putStrLn $ "initializing " ++ (Text.unpack pluginName)
       addPluginRouteFn plugins pluginName baseURI pluginToPathSegments
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
      isOk c = isAlphaChar c || isDigitChar c || elem c (":@$-_.~" :: String)

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

-- | serve requests using the 'Plugins' handle
--
-- NOTE: 
serve :: Plugins theme n hook config st -- ^ 'Plugins' handle
      -> PluginName -- ^ name of the plugin to handle this request
      -> [Text]     -- ^ unconsume path segments to pass to handler
      -> IO (Either String n)
serve plugins@(Plugins tvp) prefix path =
    do ps <- atomically $ readTVar tvp
       let phs = pluginsHandler ps
       case Map.lookup prefix phs of
         Nothing  -> return $ Left  $ "Invalid plugin prefix: " ++ Text.unpack prefix
         (Just h) -> return $ Right $ (h plugins path)
