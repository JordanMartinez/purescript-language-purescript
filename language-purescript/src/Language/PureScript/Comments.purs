module Language.PureScript.Comments where

import Prelude

import Codec.Json.Unidirectional.Value (altAccumulate, toObjSingleton, toString)
import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Comment
  = LineComment String
  | BlockComment String

derive instance Eq Comment
derive instance Ord Comment
derive instance Generic Comment _
instance Show Comment where
  show x = genericShow x

fromComment :: Comment -> Json
fromComment = case _ of
  LineComment str -> Json.fromObjSingleton "LineComment" (Json.fromString str)
  BlockComment str -> Json.fromObjSingleton "BlockComment" (Json.fromString str)

toComment :: Json -> Either Json.DecodeError Comment
toComment = altAccumulate decodeLineComment decodeBlockComment
  where
  decodeLineComment = toObjSingleton "LineComment" (toString >>> map LineComment)
  decodeBlockComment = toObjSingleton "BlockComment" (toString >>> map BlockComment)
