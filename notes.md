
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


When is a plugin in effect?

 1. when a preprocessor gets called

    - this happens in the core clckwrks code so it can't have preknowledge of the plugin

 2. when it is handling a route

 3. when so other plugin calls its methods

 4. on shutdown

 5. by a template the requires a specific plugin



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


If we want to add plugins at runtime, then we also need to be able to add routes at runtime. But, the static types would appear to make that tricky. However, by using the prefix perhaps we can know what plugin should be providing the route fn.

The pre-processor extensions can rely on resources that only exist in the context of the plugin. For example, looking up some information in a local state and generating a link.

But that is a bit interesting, because we can have a bunch of different preprocessers, each with their own context. So, how does that work? Seems most sensible that the preprocessors all have the same, more general type, and internally they can use their `runPluginT` functions to flatten the structure?

When is a plugin in context really even used?

 - pre-processor
 - show a plugin specific page

~~~~{.haskell}
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
~~~~

Initializing a plugin generally has side effects. For example, it adds
additional preprocessors to 'PluginsState'. But it can also do things
like open a database, and may need to register finalization actions.

~~~~{.haskell}
initPlugin :: Plugin url st -> StateT PluginsState IO st
initPlugin Plugin{..} =
    pluginInit
~~~~

Themes
----------

Themes are similar to plugins in that we want to be able to load
previously unknown themes at runtime via one-click install. So the
same STM requires are in place.

Unlike plugins, we generally only want to allow one theme to be active at a time.

If we are going to allow the theme to be updated at anytime, then that
means plugins shouldn't cache a specific page template function.
