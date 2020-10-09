{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, QuasiQuotes, OverloadedStrings #-}
module Main where

import Control.Monad (msum)
import Control.Monad.Trans
import Web.Plugins.Core
import MyPlugin
import ClckPlugin
import Happstack.Server
import Happstack.Server.HSP.HTML
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import DefaultTheme (theme)
import Theme (themeTemplate)
import Language.Haskell.HSX.QQ (hsx)
import HSP
import HSP.Monad

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main =
  withPlugins () () $ \plugins ->
    do initPlugin plugins "" clckPlugin
       initPlugin plugins "" myPlugin
       setTheme plugins (Just theme)
       hooks <- getPostHooks plugins
       sequence_ hooks
       simpleHTTP nullConf (route plugins)

instance EmbedAsAttr (ServerPartT IO) (Attr LazyText.Text (IO Text.Text)) where
  asAttr (n := v) =
    do v' <- XMLGenT (liftIO v)
       asAttr (n := v)

route plugins = msum
   [ nullDir >> do
       (Just clckShowFn) <- getPluginRouteFn plugins (pluginName clckPlugin)
       (Just myShowFn)   <- getPluginRouteFn plugins (pluginName myPlugin)
       themeTemplate plugins "home" () [hsx|
                  <ul>
                    <li><a href=(myShowFn MyUrl [])>my plugin</a></li>
                    <li><a href=(clckShowFn ViewPage [])>clck plugin</a></li>
                  </ul> |]
   , path $ \p -> do
       ps <- fmap rqPaths askRq
       r <- liftIO $ serve plugins p (map Text.pack ps)
       case r of
         (Left e) -> internalServerError $ toResponse e
         (Right sp) -> sp
   ]
