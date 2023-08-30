module Language.PureScript.Comments where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import JSON (JSON)
import JSON as JSON
import JSON.ExtraCodecs (toJObject, toString)
import JSON.Object as JO

data Comment
  = LineComment String
  | BlockComment String

derive instance Eq Comment
derive instance Ord Comment
derive instance Generic Comment _
instance Show Comment where
  show x = genericShow x

commentJSON :: Comment -> JSON
commentJSON = case _ of
  LineComment str -> JSON.fromJObject $ JO.insert "LineComment" (JSON.fromString str) JO.empty
  BlockComment str -> JSON.fromJObject $ JO.insert "BlockComment" (JSON.fromString str) JO.empty

jsonComment :: JSON -> Either String Comment
jsonComment j = do
  jo <- toJObject j
  decodeLineComment jo <|> decodeBlockComment jo
  where
  decodeLineComment jo = (note "required key 'LineComment' missing, " $ JO.lookup "LineComment" jo) >>= toString >>> map LineComment
  decodeBlockComment jo = (note "required key 'BlockComment' missing, " $ JO.lookup "BlockComment" jo) >>= toString >>> map BlockComment
