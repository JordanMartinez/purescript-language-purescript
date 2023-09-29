module Language.PureScript.Names where

import Prelude

import Codec.Json.Unidirectional.Value as Json
import Control.Monad.Supply.Class (class MonadSupply, fresh)
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (class Foldable, class Traversable)
import Data.Tuple (Tuple(..))
import Language.PureScript.AST.SourcePos (SourcePos(..), toSourcePos, fromSourcePos)
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)

-- | A sum of the possible name types, useful for error and lint messages.
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

getIdentName :: Name -> Maybe Ident
getIdentName (IdentName name) = Just name
getIdentName _ = Nothing

getValOpName :: Name -> Maybe (OpName ValueOpName)
getValOpName (ValOpName name) = Just name
getValOpName _ = Nothing

getTypeName :: Name -> Maybe (ProperName TypeName)
getTypeName (TyName name) = Just name
getTypeName _ = Nothing

getTypeOpName :: Name -> Maybe (OpName TypeOpName)
getTypeOpName (TyOpName name) = Just name
getTypeOpName _ = Nothing

getDctorName :: Name -> Maybe (ProperName ConstructorName)
getDctorName (DctorName name) = Just name
getDctorName _ = Nothing

getClassName :: Name -> Maybe (ProperName ClassName)
getClassName (TyClassName name) = Just name
getClassName _ = Nothing

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

unusedIdent :: String
unusedIdent = "$__unused"

runIdent :: Ident -> String
runIdent (Ident i) = i
runIdent (GenIdent Nothing n) = "$" <> show n
runIdent (GenIdent (Just name) n) = "$" <> name <> show n
runIdent UnusedIdent = unusedIdent
runIdent (InternalIdent _) = unsafeCrashWith "unexpected InternalIdent"

showIdent :: Ident -> String
showIdent = runIdent

freshIdent :: forall m. MonadSupply m => String -> m Ident
freshIdent name = GenIdent (Just name) <$> fresh

freshIdent' :: forall m. MonadSupply m => m Ident
freshIdent' = GenIdent Nothing <$> fresh

isPlainIdent :: Ident -> Boolean
isPlainIdent (Ident _) = true
isPlainIdent _ = false

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

runOpName :: forall a. OpName a -> String
runOpName = unwrap

fromOpName :: forall a. OpName a -> Json
fromOpName = unwrap >>> Json.fromString

toOpName :: forall a. Json -> Either Json.DecodeError (OpName a)
toOpName = Json.toString >>> coerce

showOp :: forall a. OpName a -> String
showOp op = "(" <> runOpName op <> ")"

-- | The closed set of operator alias types.
data OpNameType

foreign import data ValueOpName :: OpNameType
foreign import data TypeOpName :: OpNameType
foreign import data AnyOpName :: OpNameType

eraseOpName :: forall a. OpName a -> OpName AnyOpName
eraseOpName = OpName <<< runOpName

coerceOpName :: forall a b. OpName a -> OpName b
coerceOpName = OpName <<< runOpName

-- | Proper names, i.e. capitalized names for e.g. module names, type/data constructors.
newtype ProperName :: ProperNameType -> Type
newtype ProperName a = ProperName String

derive instance Eq (ProperName a)
derive instance Ord (ProperName a)
derive instance Newtype (ProperName a) _
derive instance Generic (ProperName a) _

instance Show (ProperName a) where
  show x = genericShow x

runProperName :: forall a. ProperName a -> String
runProperName = unwrap

fromProperName :: forall a. ProperName a -> Json
fromProperName = unwrap >>> Json.fromString

toProperName :: forall a. Json -> Either Json.DecodeError (ProperName a)
toProperName = Json.toString >>> coerce

-- | The closed set of proper name types.
data ProperNameType

foreign import data TypeName :: ProperNameType
foreign import data ConstructorName :: ProperNameType
foreign import data ClassName :: ProperNameType
foreign import data Namespace :: ProperNameType

-- | Coerces a ProperName from one ProperNameType to another. This should be used
-- | with care, and is primarily used to convert ClassNames into TypeNames after
-- | classes have been desugared.
coerceProperName :: forall a b. ProperName a -> ProperName b
coerceProperName = ProperName <<< runProperName

-- | Module names
newtype ModuleName = ModuleName String

derive instance Eq ModuleName
derive instance Ord ModuleName
derive instance Newtype ModuleName _
derive instance Generic ModuleName _
instance Show ModuleName where
  show x = genericShow x

runModuleName :: ModuleName -> String
runModuleName (ModuleName name) = name

moduleNameFromString :: String -> ModuleName
moduleNameFromString = ModuleName

isBuiltinModuleName :: ModuleName -> Boolean
isBuiltinModuleName (ModuleName mn) = mn == "Prim" || (isJust $ String.stripPrefix (Pattern "Prim.") mn)

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

-- pattern ByNullSourcePos :: QualifiedBy
-- pattern ByNullSourcePos = BySourcePos (SourcePos 0 0)

byNullSourcePos :: QualifiedBy
byNullSourcePos = BySourcePos (SourcePos { line: 0, column: 0 })

isBySourcePos :: QualifiedBy -> Boolean
isBySourcePos (BySourcePos _) = true
isBySourcePos _ = false

byMaybeModuleName :: Maybe ModuleName -> QualifiedBy
byMaybeModuleName (Just mn) = ByModuleName mn
byMaybeModuleName Nothing = byNullSourcePos

toMaybeModuleName :: QualifiedBy -> Maybe ModuleName
toMaybeModuleName (ByModuleName mn) = Just mn
toMaybeModuleName (BySourcePos _) = Nothing

-- | Note: this instance isn't defined in the PureScript compiler.
-- | as it appears within the instance of `Qualified a`.
fromQualifiedBy :: QualifiedBy -> Json
fromQualifiedBy = case _ of
  ByModuleName mn -> fromModuleName mn
  BySourcePos ss -> fromSourcePos ss

-- | Note: this instance isn't defined in the PureScript compiler.
-- | as it appears within the instance of `Qualified a`.
toQualifiedBy :: Json -> Either Json.DecodeError QualifiedBy
toQualifiedBy = (byModule `Json.altAccumulate` bySourcePos) `Json.altAccumulate` byMaybeModuleName'
  where
  byModule j = ByModuleName <$> toModuleName j
  bySourcePos j = BySourcePos <$> toSourcePos j
  byMaybeModuleName' = Json.toNullDefaultOrA byNullSourcePos byModule

-- | A qualified name, i.e. a name with an optional module name
data Qualified a = Qualified QualifiedBy a

derive instance Eq a => Eq (Qualified a)
derive instance Ord a => Ord (Qualified a)
derive instance Generic (Qualified a) _
instance Show a => Show (Qualified a) where
  show x = genericShow x

derive instance Functor Qualified
derive instance Foldable Qualified
derive instance Traversable Qualified

showQualified :: forall a. (a -> String) -> Qualified a -> String
showQualified f (Qualified (BySourcePos _) a) = f a
showQualified f (Qualified (ByModuleName name) a) = runModuleName name <> "." <> f a

getQual :: forall a. Qualified a -> Maybe ModuleName
getQual (Qualified qb _) = toMaybeModuleName qb

-- | Provide a default module name, if a name is unqualified
qualify :: forall a. ModuleName -> Qualified a -> Tuple ModuleName a
qualify m (Qualified (BySourcePos _) a) = Tuple m a
qualify _ (Qualified (ByModuleName m) a) = Tuple m a

-- | Makes a qualified value from a name and module name.
mkQualified :: forall a. a -> ModuleName -> Qualified a
mkQualified name mn = Qualified (ByModuleName mn) name

-- | Remove the module name from a qualified name
disqualify :: forall a. Qualified a -> a
disqualify (Qualified _ a) = a

-- | Remove the qualification from a value when it is qualified with a particular
-- | module name.
disqualifyFor :: forall a. Maybe ModuleName -> Qualified a -> Maybe a
disqualifyFor mn (Qualified qb a) | mn == toMaybeModuleName qb = Just a
disqualifyFor _ _ = Nothing

-- | Checks whether a qualified value is actually qualified with a module reference
isQualified :: forall a. Qualified a -> Boolean
isQualified (Qualified (BySourcePos _) _) = false
isQualified _ = true

-- | Checks whether a qualified value is not actually qualified with a module reference
isUnqualified :: forall a. Qualified a -> Boolean
isUnqualified = not <<< isQualified

-- | Checks whether a qualified value is qualified with a particular module
isQualifiedWith :: forall a. ModuleName -> Qualified a -> Boolean
isQualifiedWith mn (Qualified (ByModuleName mn') _) = mn == mn'
isQualifiedWith _ _ = false

fromQualified :: forall a. (a -> Json) -> Qualified a -> Json
fromQualified f (Qualified by a) = Json.fromArray2 (fromQualifiedBy by) (f a)

toQualified :: forall a. (Json -> Either Json.DecodeError a) -> Json -> Either Json.DecodeError (Qualified a)
toQualified f = Json.toArray2 toQualifiedBy f Qualified
