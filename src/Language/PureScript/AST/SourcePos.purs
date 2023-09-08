module Language.PureScript.AST.SourcePos where

import Prelude

import Codec.Json.Unidirectional.Value (fromArray, fromArray2, fromRecordN, fromRequired, toArray, toArray2, toInt, toRecordN, toRequired, toString)
import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Language.PureScript.Comments (Comment, fromComment, toComment)

-- | Source annotation - position information and comments.
newtype SourceAnn = SourceAnn
  { span :: SourceSpan
  , comments :: Array Comment
  }

derive instance Eq SourceAnn
derive instance Newtype SourceAnn _
derive instance Generic SourceAnn _
instance Show SourceAnn where
  show x = genericShow x

fromSourceAnn :: SourceAnn -> Json
fromSourceAnn (SourceAnn { span, comments }) =
  fromArray2 (fromSourceSpan span) (fromArray fromComment comments)

toSourceAnn :: Json -> Either Json.DecodeError SourceAnn
toSourceAnn =
  toArray2 toSourceSpan (toArray toComment) \span comments ->
    SourceAnn { span, comments }

-- | Source position information
newtype SourcePos = SourcePos
  { line :: Int
  , column :: Int
  }

derive instance Eq SourcePos
derive instance Ord SourcePos
derive instance Newtype SourcePos _
derive instance Generic SourcePos _
instance Show SourcePos where
  show x = genericShow x

fromSourcePos :: SourcePos -> Json
fromSourcePos (SourcePos { line, column }) =
  fromArray2 (Json.fromInt line) (Json.fromInt column)

toSourcePos :: Json -> Either Json.DecodeError SourcePos
toSourcePos = toArray2 toInt toInt \line column ->
  SourcePos { line, column }

newtype SourceSpan = SourceSpan
  { name :: String
  , start :: SourcePos
  , end :: SourcePos
  }

derive instance Eq SourceSpan
derive instance Ord SourceSpan
derive instance Newtype SourceSpan _
derive instance Generic SourceSpan _
instance Show SourceSpan where
  show x = genericShow x

fromSourceSpan :: SourceSpan -> Json
fromSourceSpan = fromRecordN SourceSpan
  { name: fromRequired Json.fromString
  , start: fromRequired fromSourcePos
  , end: fromRequired fromSourcePos
  }

toSourceSpan :: Json -> Either Json.DecodeError SourceSpan
toSourceSpan = toRecordN SourceSpan
  { name: toRequired toString
  , start: toRequired toSourcePos
  , end: toRequired toSourcePos
  }

nullSourcePos :: SourcePos
nullSourcePos = SourcePos { line: 0, column: 0 }

nullSourceSpan :: SourceSpan
nullSourceSpan = SourceSpan { name: "", start: nullSourcePos, end: nullSourcePos }

nullSourceAnn :: SourceAnn
nullSourceAnn = SourceAnn { span: nullSourceSpan, comments: [] }
