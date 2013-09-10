{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module DefaultTheme where

import Data.Text (Text)
import Happstack.Server
import HSP
import Theme


theme :: Theme
theme = Theme
    { themeName      = "default-theme"
    , _themeTemplate = pageTemplate
    }


pageTemplate :: ( EmbedAsChild (ServerPartT IO) headers
                , EmbedAsChild (ServerPartT IO) body) =>
                Text    -- ^ page title
             -> headers -- ^ extra elements to add to \<head\>
             -> body    -- ^ elements to insert in \<body\>
             -> XMLGenT (ServerPartT IO) XML
pageTemplate title hdrs bdy =
    <html>
     <head>
      <title><% title %></title>
      <% hdrs %>
     </head>
     <body>
      <% bdy %>
     </body>
    </html>