module Core where

import Data.Text (Text)
import Happstack.Server
import Theme (Theme)
import Web.Plugins.Core

type URLFn url = url -> [(Text, Maybe Text)] -> Text

type ExamplePlugins    = Plugins    Theme (ServerPart Response) (IO ()) () ()
type ExamplePlugin url = Plugin url Theme (ServerPart Response) (IO ()) () ()

