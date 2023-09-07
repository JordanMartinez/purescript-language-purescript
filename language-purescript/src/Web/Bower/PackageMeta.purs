module Web.Bower.PackageMeta where

import Prelude

import Codec.Json.Unidirectional.Value (DecodeError(..), FromProp(..), fromOption, fromOptionArray, fromOptionAssocArray, fromRecordN, fromRequired, toBoolean, toOption, toOptionArray, toOptionAssocArray, toOptionDefault, toRecordN, toRequired, toString)
import Codec.Json.Unidirectional.Value as Json
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.CodePoint.Unicode (isAscii, isDecDigit, isLower)
import Data.Either (Either, note)
import Data.Enum (class BoundedEnum, class Enum, enumFromTo)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Foldable (foldr, traverse_)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String (CodePoint, Pattern(..), fromCodePointArray, toCodePointArray)
import Data.String as String
import Data.Tuple (Tuple(..))
import Safe.Coerce (coerce)

---------------------
-- Data types

-- | A data type representing the data stored in a bower.json package manifest
-- file.
--
-- Note that the 'encodeJson' / 'FromJSON' instances don't exactly match; for
-- example, it is not always the case that decoding from Json and then encoding
-- to Json will give you the exact same Json that you started with. However, if
-- you start with a PackageMeta value, encode to Json, and then decode, you
-- should always get the same value back.
newtype PackageMeta = PackageMeta
  { name :: PackageName
  , description :: Maybe String
  , main :: Array String
  , moduleType :: Array ModuleType
  , licence :: Array String
  , ignore :: Array String
  , keywords :: Array String
  , authors :: Array Author
  , homepage :: Maybe String
  , repository :: Maybe Repository
  , dependencies :: Array (Tuple PackageName VersionRange)
  , devDependencies :: Array (Tuple PackageName VersionRange)
  , resolutions :: Array (Tuple PackageName Version)
  , private :: Boolean
  }

derive instance Eq PackageMeta
derive instance Ord PackageMeta
derive instance Newtype PackageMeta _
derive instance Generic PackageMeta _
instance Show PackageMeta where
  show x = genericShow x

packageMetaJSON :: PackageMeta -> Json
packageMetaJSON = fromRecordN PackageMeta
  { name: fromRequired packageNameJSON
  , description: fromOption Json.fromString
  , main: fromOptionArray Json.fromString
  , moduleType: fromOptionArray moduleTypeJSON
  , licence: fromOptionArray Json.fromString
  , ignore: fromOptionArray Json.fromString
  , keywords: fromOptionArray Json.fromString
  , authors: fromOptionArray authorJSON
  , homepage: fromOption Json.fromString
  , repository: fromOption repositoryJSON
  , dependencies: fromOptionAssocArray unwrap versionRangeJSON
  , devDependencies: fromOptionAssocArray unwrap versionRangeJSON
  , resolutions: fromOptionAssocArray unwrap versionJSON
  , private: FromProp \b ->
      if b then Just $ Tuple Nothing $ Json.fromBoolean b else Nothing
  }

jsonPackageMeta :: Json -> Either Json.DecodeError PackageMeta
jsonPackageMeta =
  toRecordN PackageMeta
    { name: toRequired jsonPackageName
    , description: toOption toString
    , main: toOptionArray toString
    , moduleType: toOptionArray jsonModuleType
    , licence: toOptionArray toString
    , ignore: toOptionArray toString
    , keywords: toOptionArray toString
    , authors: toOptionArray jsonAuthor
    , homepage: toOption toString
    , repository: toOption jsonRepository
    , dependencies: toOptionAssocArray jsonPkgName jsonVersionRange
    , devDependencies: toOptionAssocArray jsonPkgName jsonVersionRange
    , resolutions: toOptionAssocArray jsonPkgName jsonVersion
    , private: toOptionDefault false toBoolean
    }
  where
  jsonPkgName = note (DecodeError "Invalid package name") <<< parsePackageName

-- | A valid package name for a Bower package.
newtype PackageName = PackageName String

derive instance Eq PackageName
derive instance Ord PackageName
derive instance Newtype PackageName _
derive instance Generic PackageName _
instance Show PackageName where
  show x = genericShow x

packageNameJSON :: PackageName -> Json
packageNameJSON = unwrap >>> Json.fromString

jsonPackageName :: Json -> Either Json.DecodeError PackageName
jsonPackageName = toString >=> parsePackageName >>> note (DecodeError "Invalid package name")

parsePackageName :: String -> Maybe PackageName
parsePackageName = mkPackageName
  where
  mkPackageName :: String -> Maybe PackageName
  mkPackageName = validateAll validators <<< toCodePointArray
    where
    dashOrDot = toCodePointArray "-."

    validateAll :: Array (Array CodePoint -> Boolean) -> Array CodePoint -> Maybe PackageName
    validateAll vs x = traverse_ (validateWith x) vs *> pure (PackageName $ String.fromCodePointArray x)

    validateWith :: Array CodePoint -> (Array CodePoint -> Boolean) -> Maybe (Array CodePoint)
    validateWith x p
      | p x = Just x
      | otherwise = Nothing
    validChar c = isAscii c && (isLower c || isDecDigit c || c `Array.elem` dashOrDot)

    validators :: Array (Array CodePoint -> Boolean)
    validators =
      [ not <<< Array.null
      , Array.all validChar
      , Array.head >>> maybe false (\x -> not $ x `Array.elem` dashOrDot)
      , Array.last >>> maybe false (\x -> not $ x `Array.elem` dashOrDot)
      , not <<< String.contains (Pattern "--") <<< fromCodePointArray
      , not <<< String.contains (Pattern "..") <<< fromCodePointArray
      , Array.length >>> (_ <= 50)
      ]

newtype Author = Author
  { name :: String
  , email :: Maybe String
  , homepage :: Maybe String
  }

derive instance Eq Author
derive instance Ord Author
derive instance Newtype Author _
derive instance Generic Author _
instance Show Author where
  show x = genericShow x

authorJSON :: Author -> Json
authorJSON = fromRecordN Author
  { name: fromRequired Json.fromString
  , email: fromOption Json.fromString
  , homepage: fromOption Json.fromString
  }

jsonAuthor :: Json -> Either Json.DecodeError Author
jsonAuthor j = decodeAuthorString j <|> decodeAuthorObj j
  where
  decodeAuthorString = toString >=> \str -> do
    let
      Tuple email s1 =
        str
          # String.split (Pattern " ")
          # List.fromFoldable
          # takeDelim "<" ">"
      Tuple homepage s2 = takeDelim "(" ")" s1
    pure $ Author
      { name: List.intercalate " " s2
      , email
      , homepage
      }
    where
    takeDelim l r = foldr go (Tuple Nothing Nil)
      where
      go str (Tuple (Just x) strs) =
        Tuple (Just x) (str List.: strs)
      go str (Tuple Nothing strs) =
        case stripWrapper l r str of
          Just str' -> Tuple (Just str') strs
          Nothing -> Tuple Nothing $ str List.: strs

    stripWrapper l r = String.stripPrefix (Pattern l) >=> String.stripSuffix (Pattern r)

  decodeAuthorObj :: Json -> Either Json.DecodeError Author
  decodeAuthorObj = toRecordN Author
    { name: toRequired toString
    , email: toOption toString
    , homepage: toOption toString
    }

-- | See: <https://github.com/bower/bower.json-spec#moduletype>
data ModuleType
  = Globals
  | AMD
  | Node
  | ES6
  | YUI

derive instance Eq ModuleType
derive instance Ord ModuleType
derive instance Generic ModuleType _
instance Show ModuleType where
  show x = genericShow x

instance Enum ModuleType where
  succ x = genericSucc x
  pred x = genericPred x

instance Bounded ModuleType where
  bottom = genericBottom
  top = genericTop

instance BoundedEnum ModuleType where
  cardinality = genericCardinality
  toEnum x = genericToEnum x
  fromEnum x = genericFromEnum x

moduleTypes :: Map String ModuleType
moduleTypes =
  Map.fromFoldable
    $ map (\t -> Tuple (String.toLower $ show t) t)
    $ (enumFromTo bottom top :: Array ModuleType)

moduleTypeJSON :: ModuleType -> Json
moduleTypeJSON = show >>> String.toLower >>> Json.fromString

jsonModuleType :: Json -> Either Json.DecodeError ModuleType
jsonModuleType = toString >=> flip Map.lookup moduleTypes >>> note (DecodeError "Key not found in 'moduleTypes' map")

newtype Repository = Repository
  { url :: String
  , "type" :: String
  }

derive instance Eq Repository
derive instance Ord Repository
derive instance Newtype Repository _
derive instance Generic Repository _
instance Show Repository where
  show x = genericShow x

repositoryJSON :: Repository -> Json
repositoryJSON = fromRecordN Repository
  { url: fromRequired Json.fromString
  , type: fromRequired Json.fromString
  }

jsonRepository :: Json -> Either Json.DecodeError Repository
jsonRepository = toRecordN Repository
  { url: toRequired toString
  , type: toRequired toString
  }

newtype Version = Version String

derive instance Eq Version
derive instance Ord Version
derive instance Newtype Version _
derive instance Generic Version _
instance Show Version where
  show x = genericShow x

versionJSON :: Version -> Json
versionJSON = unwrap >>> Json.fromString

jsonVersion :: Json -> Either Json.DecodeError Version
jsonVersion = coerce <<< toString

newtype VersionRange = VersionRange String

derive instance Eq VersionRange
derive instance Ord VersionRange
derive instance Newtype VersionRange _
derive instance Generic VersionRange _
instance Show VersionRange where
  show x = genericShow x

versionRangeJSON :: VersionRange -> Json
versionRangeJSON = unwrap >>> Json.fromString

jsonVersionRange :: Json -> Either Json.DecodeError VersionRange
jsonVersionRange = coerce <<< toString

data BowerError
  = InvalidPackageName PackageNameError
  | InvalidModuleType String

derive instance Eq BowerError
derive instance Ord BowerError
derive instance Generic BowerError _
instance Show BowerError where
  show x = genericShow x

data PackageNameError
  = NotEmpty
  | TooLong Int
  | InvalidChars String
  | RepeatedSeparators
  | MustNotBeginSeparator
  | MustNotEndSeparator

derive instance Eq PackageNameError
derive instance Ord PackageNameError
derive instance Generic PackageNameError _
instance Show PackageNameError where
  show x = genericShow x
