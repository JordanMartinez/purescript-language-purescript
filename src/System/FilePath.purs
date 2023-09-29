module System.FilePath where

import Prelude

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..))
import Data.String as String

makeRelative :: String -> String -> String
makeRelative relPath fileName =
  fromMaybe fileName $ String.stripPrefix (Pattern relPath) fileName
