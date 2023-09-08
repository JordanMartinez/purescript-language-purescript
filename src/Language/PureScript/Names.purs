module Language.PureScript.Names where

import Prelude

import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (class Foldable, class Traversable)
import Language.PureScript.AST.SourcePos (SourcePos(..), toSourcePos, fromSourcePos)
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

fromInternalIdentData :: InternalIdentData -> Json
fromInternalIdentData = case _ of
  RuntimeLazyFactory -> Json.fromObjSingleton "RuntimeLazyFactory" Json.fromJNull
  Lazy str -> Json.fromObjSingleton "Lazy" (Json.fromString str)

toInternalIdentData :: Json -> Either Json.DecodeError InternalIdentData
toInternalIdentData = Json.altAccumulate runtimeLazy lazyStr
  where
  runtimeLazy = Json.toObjSingleton "RuntimeLazyFactory" (const $ pure RuntimeLazyFactory)
  lazyStr = Json.toObjSingleton "Lazy" (Json.toString >>> map Lazy)

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

fromIdent :: Ident -> Json
fromIdent = case _ of
  Ident str -> Json.fromObjSingleton "Ident" $ Json.fromString str
  GenIdent mbStr i -> Json.fromObjSingleton "GenIdent"
    $ Json.fromArray2 (Json.fromNullNothingOrJust Json.fromString mbStr) (Json.fromInt i)
  UnusedIdent -> Json.fromObjSingleton "UnusedIdent" Json.fromJNull
  InternalIdent iid -> Json.fromObjSingleton "InternalIdent" $ fromInternalIdentData iid

toIdent :: Json -> Either Json.DecodeError Ident
toIdent = ((jIdent `Json.altAccumulate` jGenIdent) `Json.altAccumulate` jUnusedIdent) `Json.altAccumulate` jInternalIdent
  where
  jIdent = Json.toObjSingleton "Ident" (Json.toString >>> map Ident)
  jGenIdent = Json.toObjSingleton "GenIdent" (Json.toArray2 (Json.toNullNothingOrJust Json.toString) Json.toInt GenIdent)
  jUnusedIdent = Json.toObjSingleton "UnusedIdent" (const $ pure UnusedIdent)
  jInternalIdent = Json.toObjSingleton "InternalIdent" (toInternalIdentData >>> map InternalIdent)

-- | Operator alias names.
newtype OpName :: OpNameType -> Type
newtype OpName a = OpName String

derive instance Eq (OpName a)
derive instance Ord (OpName a)
derive instance Newtype (OpName a) _
derive instance Generic (OpName a) _
instance Show (OpName a) where
  show x = genericShow x

fromOpName :: forall a. OpName a -> Json
fromOpName = unwrap >>> Json.fromString

toOpName :: forall a. Json -> Either Json.DecodeError (OpName a)
toOpName = Json.toString >>> coerce

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

fromProperName :: forall a. ProperName a -> Json
fromProperName = unwrap >>> Json.fromString

toProperName :: forall a. Json -> Either Json.DecodeError (ProperName a)
toProperName = Json.toString >>> coerce

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

fromModuleName :: ModuleName -> Json
fromModuleName = unwrap >>> String.split (Pattern ".") >>> Json.fromArray Json.fromString

toModuleName :: Json -> Either Json.DecodeError ModuleName
toModuleName = Json.toArray Json.toString >>> map (Array.intercalate "." >>> wrap)

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
fromQualifiedBy :: QualifiedBy -> Json
fromQualifiedBy = case _ of
  ByModuleName mn -> fromModuleName mn
  BySourcePos ss -> fromSourcePos ss

-- | Note: this instance isn't defined in the PureScript compiler.
-- | as it appears within the instance of `Qualified a`.
toQualifiedBy :: Json -> Either Json.DecodeError QualifiedBy
toQualifiedBy = (byModule `Json.altAccumulate` bySourcePos) `Json.altAccumulate` byMaybeModuleName
  where
  byModule j = ByModuleName <$> toModuleName j
  bySourcePos j = BySourcePos <$> toSourcePos j
  byMaybeModuleName = Json.toNullDefaultOrA byNullSourcePos byModule

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

fromQualified :: forall a. (a -> Json) -> Qualified a -> Json
fromQualified f (Qualified by a) = Json.fromArray2 (fromQualifiedBy by) (f a)

toQualified :: forall a. (Json -> Either Json.DecodeError a) -> Json -> Either Json.DecodeError (Qualified a)
toQualified f = Json.toArray2 toQualifiedBy f Qualified
