{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, TemplateHaskell, OverloadedStrings #-}
module Web.Plugins.Dynamic
    ( loadPlugin
    , loadPlugin_
    , loadTheme
    )
    where

import           Data.Text           (Text)
import qualified Data.Text           as Text
import           System.Plugins.Load (LoadStatus(..), load_)
import           Web.Plugins.Core    (Plugins, setTheme)

loadPlugin :: Plugins theme a hook config st
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

loadPlugin_ :: Plugins theme a hook config st
           -> Text        -- ^ baseURI
           -> FilePath    -- ^ object file .hi
           -> [FilePath]  -- ^ include paths
           -> IO ()
loadPlugin_ plugins baseURI obj incs =
    do me <- loadPlugin plugins baseURI obj incs
       case me of
         Nothing -> return ()
         (Just e) -> error $ Text.unpack e

loadTheme :: Plugins theme a hook config st
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
