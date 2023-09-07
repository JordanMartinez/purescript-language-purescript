module Language.PureScript.Comments where

import Prelude

import Codec.Json.Unidirectional.Value (toJObject, toString, underKey)
import Codec.Json.Unidirectional.Value as Json
import Control.Alt ((<|>))
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

commentJSON :: Comment -> Json
commentJSON = case _ of
  LineComment str -> Json.fromObjSingleton "LineComment" (Json.fromString str)
  BlockComment str -> Json.fromObjSingleton "BlockComment" (Json.fromString str)

jsonComment :: Json -> Either Json.DecodeError Comment
jsonComment j = do
  jo <- toJObject j
  decodeLineComment jo <|> decodeBlockComment jo
  where
  decodeLineComment = underKey "LineComment" (toString >>> map LineComment)
  decodeBlockComment = underKey "BlockComment" (toString >>> map BlockComment)
