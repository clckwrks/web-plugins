{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Theme where

import Web.Plugin.Core (Plugins, getTheme)
import Data.Text (Text)
import HSP hiding (escape)
import HSP.ServerPartT
import Happstack.Server
import Happstack.Server.HSP.HTML ()

type ThemeName = Text

data Theme = Theme
    { themeName      :: ThemeName
    , _themeTemplate :: ( EmbedAsChild (ServerPartT IO) headers
                        , EmbedAsChild (ServerPartT IO) body) =>
                       Text    -- ^ page title
                     -> headers -- ^ extra elements to add to \<head\>
                     -> body    -- ^ elements to insert in \<body\>
                     -> XMLGenT (ServerPartT IO) XML
    }

themeTemplate :: ( EmbedAsChild (ServerPartT IO) headers
                 , EmbedAsChild (ServerPartT IO) body
                 ) =>
                 Plugins Theme (ServerPart Response)
              -> Text
              -> headers
              -> body
              -> ServerPartT IO Response
themeTemplate plugins title headers body =
    do mTheme <- getTheme plugins
       case mTheme of
         Nothing -> escape $ internalServerError $ toResponse $ "No theme package is loaded."
         (Just theme) -> fmap toResponse $ unXMLGenT $ (_themeTemplate theme) title headers body