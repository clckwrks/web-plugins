module Core where

import Data.Text (Text)

type URLFn url = url -> [(Text, Text)] -> Text

