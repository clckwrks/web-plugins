{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad.Trans (MonadIO(liftIO))
import Web.Plugin.Core
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Happstack.Server
import System.Plugins.Load
import System.Environment
import Theme


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main =
    let baseURI = "http://localhost:8000"
    in
      do (themeObj : pluginObjs) <- getArgs
         withPlugins $ \plugins ->
             do loadTheme plugins themeObj []
                mapM_ (\obj -> loadPlugin_ plugins baseURI obj []) pluginObjs
                simpleHTTP nullConf $
                          do paths <- (map Text.pack . rqPaths) <$> askRq
                             case paths of
                               (p : ps) ->
                                   do e <- liftIO $ serve plugins p ps
                                      case e of
                                        (Left e) -> notFound $ toResponse e
                                        (Right sp) -> sp
                               _ -> notFound (toResponse ())
