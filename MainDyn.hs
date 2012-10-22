{-# LANGUAGE OverloadedStrings #-}
module Main where

import Core
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Plugins.Load
import System.Environment


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

loadPlugin :: Plugins
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
                return Nothing


loadPlugin_ :: Plugins
           -> Text        -- ^ baseURI
           -> FilePath    -- ^ object file .hi
           -> [FilePath]  -- ^ include paths
           -> IO ()
loadPlugin_ plugins baseURI obj incs =
    do me <- loadPlugin plugins baseURI obj incs
       case me of
         Nothing -> return ()
         (Just e) -> error $ Text.unpack e

main :: IO ()
main =
    let baseURI = "http://localhost:8000"
    in
      do objs <- getArgs
         withPlugins $ \plugins ->
             do mapM_ (\obj -> loadPlugin_ plugins baseURI obj []) objs
                serve plugins "clck" ["ViewPage"]
                serve plugins "my" ["MyURL"]

{-
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

-}