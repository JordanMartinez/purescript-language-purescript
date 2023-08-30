module Language.PureScript.Names where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (class Foldable, class Traversable)
import JSON (JSON)
import JSON as JSON
import JSON.ExtraCodecs (fromArray, fromArray2, fromJoSingleton, fromNullOrJust, toArray, toArray2, toInt, toJObject, toJoSingleton, toNullDefaultOrA, toNullNothingOrJust, toSingleton, toString)
import JSON.Object as JO
import Language.PureScript.AST.SourcePos (SourcePos(..), jsonSourcePos, sourcePosJSON)
import Safe.Coerce (coerce)

data Name
  = IdentName Ident
  | ValOpName (OpName ValueOpName)
  | TyName (ProperName TypeName)
  | TyOpName (OpName TypeOpName)
  | DctorName (ProperName ConstructorName)
  | TyClassName (ProperName ClassName)
  | ModName ModuleName

derive instance Eq Name
derive instance Ord Name
derive instance Generic Name _
instance Show Name where
  show x = genericShow x

-- | This type is meant to be extended with any new uses for idents that come
-- | along. Adding constructors to this type is cheaper than adding them to
-- | `Ident` because functions that match on `Ident` can ignore all
-- | `InternalIdent`s with a single pattern, and thus don't have to change if
-- | a new `InternalIdentData` constructor is created.
data InternalIdentData
  -- Used by CoreFn.Laziness
  = RuntimeLazyFactory
  | Lazy String

derive instance Eq InternalIdentData
derive instance Ord InternalIdentData
derive instance Generic InternalIdentData _
instance Show InternalIdentData where
  show x = genericShow x

internalIdentDataJSON :: InternalIdentData -> JSON
internalIdentDataJSON = case _ of
  RuntimeLazyFactory -> fromJoSingleton "RuntimeLazyFactory" JSON.null
  Lazy str -> fromJoSingleton "Lazy" (JSON.fromString str)

jsonInternalIdentData :: JSON -> Either String InternalIdentData
jsonInternalIdentData j = runtimeLazy <|> lazyStr
  where
  runtimeLazy = toJoSingleton "RuntimeLazyFactory" (const $ pure RuntimeLazyFactory) j
  lazyStr = toJoSingleton "Lazy" (\j' -> note "missing key Lazy." j' >>= toString >>> map Lazy) j

-- | Names for value identifiers
data Ident
  -- | An alphanumeric identifier
  = Ident String
  -- | A generated name for an identifier
  | GenIdent (Maybe String) Int
  -- | A generated name used only for type-checking
  | UnusedIdent
  -- | A generated name used only for internal transformations
  | InternalIdent InternalIdentData

derive instance Eq Ident
derive instance Ord Ident
derive instance Generic Ident _
instance Show Ident where
  show x = genericShow x

identJSON :: Ident -> JSON
identJSON = JSON.fromJObject <<< case _ of
  Ident str -> JO.singleton "Ident" $ JSON.fromString str
  GenIdent mbStr i -> JO.singleton "GenIdent"
    $ fromArray2 (fromNullOrJust JSON.fromString mbStr) (JSON.fromInt i)
  UnusedIdent -> JO.singleton "UnusedIdent" JSON.null
  InternalIdent iid -> JO.singleton "InternalIdent" $ internalIdentDataJSON iid

jsonIdent :: JSON -> Either String Ident
jsonIdent = toJObject >=> rest
  where
  rest jo = jIdent <|> jGenIdent <|> jUnusedIdent <|> jInternalIdent
    where
    jIdent = toSingleton "Ident" (toString >>> map Ident) jo
    jGenIdent = toSingleton "GenIdent" (toArray2 (toNullNothingOrJust toString) toInt GenIdent) jo
    jUnusedIdent = toSingleton "UnusedIdent" (const $ pure UnusedIdent) jo
    jInternalIdent = toSingleton "InternalIdent" (jsonInternalIdentData >>> map InternalIdent) jo

-- | Operator alias names.
newtype OpName :: OpNameType -> Type
newtype OpName a = OpName String

derive instance Eq (OpName a)
derive instance Ord (OpName a)
derive instance Newtype (OpName a) _
derive instance Generic (OpName a) _
instance Show (OpName a) where
  show x = genericShow x

opNameJSON :: forall a. OpName a -> JSON
opNameJSON = unwrap >>> JSON.fromString

jsonOpName :: forall a. JSON -> Either String (OpName a)
jsonOpName = toString >>> coerce

data OpNameType

foreign import data ValueOpName :: OpNameType
foreign import data TypeOpName :: OpNameType
foreign import data AnyOpName :: OpNameType

-- | Proper names, i.e. capitalized names for e.g. module names, type/data constructors.
newtype ProperName :: ProperNameType -> Type
newtype ProperName a = ProperName String

derive instance Eq (ProperName a)
derive instance Ord (ProperName a)
derive instance Newtype (ProperName a) _
derive instance Generic (ProperName a) _

instance Show (ProperName a) where
  show x = genericShow x

properNameJSON :: forall a. ProperName a -> JSON
properNameJSON = unwrap >>> JSON.fromString

jsonProperName :: forall a. JSON -> Either String (ProperName a)
jsonProperName = toString >>> coerce

data ProperNameType

foreign import data TypeName :: ProperNameType
foreign import data ConstructorName :: ProperNameType
foreign import data ClassName :: ProperNameType
foreign import data Namespace :: ProperNameType

newtype ModuleName = ModuleName String

derive instance Eq ModuleName
derive instance Ord ModuleName
derive instance Newtype ModuleName _
derive instance Generic ModuleName _
instance Show ModuleName where
  show x = genericShow x

moduleNameJSON :: ModuleName -> JSON
moduleNameJSON = unwrap >>> String.split (Pattern ".") >>> fromArray JSON.fromString

jsonModuleName :: JSON -> Either String ModuleName
jsonModuleName = toArray toString >>> map (Array.intercalate "." >>> wrap)

data QualifiedBy
  = BySourcePos SourcePos
  | ByModuleName ModuleName

derive instance Eq QualifiedBy
derive instance Ord QualifiedBy
derive instance Generic QualifiedBy _
instance Show QualifiedBy where
  show x = genericShow x

byNullSourcePos :: QualifiedBy
byNullSourcePos = BySourcePos (SourcePos { line: 0, column: 0 })

-- | Note: this instance isn't defined in the PureScript compiler.
-- | as it appears within the instance of `Qualified a`.
qualifiedByJSON :: QualifiedBy -> JSON
qualifiedByJSON = case _ of
  ByModuleName mn -> moduleNameJSON mn
  BySourcePos ss -> sourcePosJSON ss

-- | Note: this instance isn't defined in the PureScript compiler.
-- | as it appears within the instance of `Qualified a`.
jsonQualifiedBy :: JSON -> Either String QualifiedBy
jsonQualifiedBy j =
  byModule j <|> bySourcePos <|> byMaybeModuleName
  where
  byModule j' = ByModuleName <$> jsonModuleName j'
  bySourcePos = BySourcePos <$> jsonSourcePos j
  byMaybeModuleName = toNullDefaultOrA byNullSourcePos byModule j

-- |
-- A qualified name, i.e. a name with an optional module name
--
data Qualified a = Qualified QualifiedBy a

derive instance Eq a => Eq (Qualified a)
derive instance Ord a => Ord (Qualified a)
derive instance Generic (Qualified a) _
instance Show a => Show (Qualified a) where
  show x = genericShow x

derive instance Functor Qualified
derive instance Foldable Qualified
derive instance Traversable Qualified

qualifiedJSON :: forall a. (a -> JSON) -> Qualified a -> JSON
qualifiedJSON f (Qualified by a) = fromArray2 (qualifiedByJSON by) (f a)

jsonQualified :: forall a. (JSON -> Either String a) -> JSON -> Either String (Qualified a)
jsonQualified f = toArray2 jsonQualifiedBy f Qualified
