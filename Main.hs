{-# LANGUAGE OverloadedStrings #-}
module Main where

import Core
import MyPlugin
import ClckPlugin

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

