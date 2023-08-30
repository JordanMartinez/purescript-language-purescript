module JSON.ExtraCodecs where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Bifunctor (lmap)
import Data.Bitraversable (bitraverse)
import Data.Either (Either(..), note)
import Data.Function.Uncurried (runFn7)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Symbol (class IsSymbol)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import JSON (JArray, JObject, JSON)
import JSON as JSON
import JSON.Array as JA
import JSON.Internal as Internal
import JSON.Object as JO
import Partial.Unsafe (unsafeCrashWith)
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail, Above, Beside, Quote, Text)
import Record as Record
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

coerce1 :: forall f n a. Coercible (f n) (f a) => (a -> n) -> f a -> f n
coerce1 _ = coerce

primMsg :: forall a b. String -> String -> a -> Either String b
primMsg exp act _ = Left $ "Expected primitive JSON value, " <> exp <> ", but got " <> act

toNull :: JSON -> Either String Unit
toNull json = runFn7 Internal._case Right (primMsg "Null" "Boolean") (primMsg "Null" "Number") (primMsg "Null" "String") (primMsg "Null" "JArray") (primMsg "Null" "JObject") json

toBoolean :: JSON -> Either String Boolean
toBoolean json = runFn7 Internal._case (primMsg "Boolean" "Null") Right (primMsg "Boolean" "Number") (primMsg "Boolean" "String") (primMsg "Boolean" "JArray") (primMsg "Boolean" "JObject") json

toNumber :: JSON -> Either String Number
toNumber json = runFn7 Internal._case (primMsg "Number" "Null") (primMsg "Number" "Boolean") Right (primMsg "Number" "String") (primMsg "Number" "JArray") (primMsg "Number" "JObject") json

toString :: JSON -> Either String String
toString json = runFn7 Internal._case (primMsg "String" "Null") (primMsg "String" "Boolean") (primMsg "String" "Number") Right (primMsg "String" "JArray") (primMsg "String" "JObject") json

toJArray :: JSON -> Either String JArray
toJArray json = runFn7 Internal._case (primMsg "JArray" "Null") (primMsg "JArray" "Boolean") (primMsg "JArray" "Number") (primMsg "JArray" "String") Right (primMsg "JArray" "JObject") json

toJObject :: JSON -> Either String JObject
toJObject json = runFn7 Internal._case (primMsg "JObject" "Null") (primMsg "JObject" "Boolean") (primMsg "JObject" "Number") (primMsg "JObject" "String") (primMsg "JObject" "JArray") Right json

toInt :: JSON -> Either String Int
toInt = toNumber >=> Int.fromNumber >>> note "Unable to convert number to Int"

tupleJSON :: forall a b. (a -> JSON) -> (b -> JSON) -> Tuple a b -> JSON
tupleJSON a' b' (Tuple a b) = fromArray2 (a' a) (b' b)

jsonTuple :: forall a b. (JSON -> Either String a) -> (JSON -> Either String b) -> JSON -> Either String (Tuple a b)
jsonTuple a' b' = toArray2 a' b' Tuple

eitherJSON :: forall a b. (a -> JSON) -> (b -> JSON) -> Either a b -> JSON
eitherJSON l' r' = case _ of
  Left l -> fromJoSingleton "Left" $ l' l
  Right r -> fromJoSingleton "Right" $ r' r

jsonEither :: forall a b. (JSON -> Either String a) -> (JSON -> Either String b) -> JSON -> Either String (Either a b)
jsonEither l' r' j = left <|> right
  where
  left = toJoSingleton "Left" (\j' -> (note "Expected key 'Left'" j') >>= l' >>> map Left) j
  right = toJoSingleton "Right" (\j' -> (note "Expected key 'Right'" j') >>= r' >>> map Right) j

fromArray :: forall a. (a -> JSON) -> Array a -> JSON
fromArray f = map f >>> JSON.fromArray

overIndex :: forall a. Int -> Either String a -> Either String a
overIndex i = lmap (append ("under index " <> show i <> ", "))

overKey :: forall a. String -> Either String a -> Either String a
overKey key = lmap (append ("under key " <> show key <> ", "))

toArray :: forall a. (JSON -> Either String a) -> JSON -> Either String (Array a)
toArray f = toArray' >=> traverseWithIndex \i -> overIndex i <<< f

toArray' :: JSON -> Either String (Array JSON)
toArray' = map Internal.toArray <$> toJArray

fromArray2 :: JSON -> JSON -> JSON
fromArray2 a b = JSON.fromArray [ a, b ]

toArray2
  :: forall a b x
   . (JSON -> Either String a)
  -> (JSON -> Either String b)
  -> (a -> b -> x)
  -> JSON
  -> Either String x
toArray2 a' b' f j = do
  arr <- toArray' j
  case arr of
    [ a, b ] ->
      f
        <$> (overIndex 0 $ a' a)
        <*> (overIndex 1 $ b' b)
    _ -> Left $ "Expected an array of 2 elements but had size: " <> show (Array.length arr)

fromArray3 :: JSON -> JSON -> JSON -> JSON
fromArray3 a b c = JSON.fromArray [ a, b, c ]

toArray3
  :: forall a b c x
   . (JSON -> Either String a)
  -> (JSON -> Either String b)
  -> (JSON -> Either String c)
  -> (a -> b -> c -> x)
  -> JSON
  -> Either String x
toArray3 a' b' c' f j = do
  arr <- toArray' j
  case arr of
    [ a, b, c ] ->
      f
        <$> (overIndex 0 $ a' a)
        <*> (overIndex 1 $ b' b)
        <*> (overIndex 2 $ c' c)
    _ -> Left $ "Expected an array of 3 elements but had size: " <> show (Array.length arr)

fromArray4 :: JSON -> JSON -> JSON -> JSON -> JSON
fromArray4 a b c d = JSON.fromArray [ a, b, c, d ]

toArray4
  :: forall a b c d x
   . (JSON -> Either String a)
  -> (JSON -> Either String b)
  -> (JSON -> Either String c)
  -> (JSON -> Either String d)
  -> (a -> b -> c -> d -> x)
  -> JSON
  -> Either String x
toArray4 a' b' c' d' f j = do
  arr <- toArray' j
  case arr of
    [ a, b, c, d ] ->
      f
        <$> (overIndex 0 $ a' a)
        <*> (overIndex 1 $ b' b)
        <*> (overIndex 2 $ c' c)
        <*> (overIndex 3 $ d' d)
    _ -> Left $ "Expected an array of 4 elements but had size: " <> show (Array.length arr)

fromArray5 :: JSON -> JSON -> JSON -> JSON -> JSON -> JSON
fromArray5 a b c d e = JSON.fromArray [ a, b, c, d, e ]

toArray5
  :: forall a b c d e x
   . (JSON -> Either String a)
  -> (JSON -> Either String b)
  -> (JSON -> Either String c)
  -> (JSON -> Either String d)
  -> (JSON -> Either String e)
  -> (a -> b -> c -> d -> e -> x)
  -> JSON
  -> Either String x
toArray5 a' b' c' d' e' f j = do
  arr <- toArray' j
  case arr of
    [ a, b, c, d, e ] ->
      f
        <$> (overIndex 0 $ a' a)
        <*> (overIndex 1 $ b' b)
        <*> (overIndex 2 $ c' c)
        <*> (overIndex 3 $ d' d)
        <*> (overIndex 4 $ e' e)
    _ -> Left $ "Expected an array of 5 elements but had size: " <> show (Array.length arr)

fromNullOrJust :: forall a. (a -> JSON) -> Maybe a -> JSON
fromNullOrJust f = maybe JSON.null f

toNullNothingOrJust :: forall a. (JSON -> Either String a) -> JSON -> Either String (Maybe a)
toNullNothingOrJust f = toNullDefaultOrA Nothing (map Just <$> f)

toNullDefaultOrA :: forall a. a -> (JSON -> Either String a) -> JSON -> Either String a
toNullDefaultOrA a f j = (a <$ toNull j) <|> f j

fromJoSingleton :: String -> JSON -> JSON
fromJoSingleton k v = JSON.fromJObject $ JO.singleton k v

toJoSingleton :: forall a. String -> (Maybe JSON -> Either String a) -> JSON -> Either String a
toJoSingleton k f = toJObject >=> (\jo -> overKey k $ f (JO.lookup k jo))

toSingleton :: forall a. String -> (JSON -> Either String a) -> JObject -> Either String a
toSingleton k to = JO.lookup k >>> (note $ "Key '" <> k <> "' not found") >=> (to >>> overKey k)

fromPropArray :: Array (Tuple String JSON) -> JSON
fromPropArray = JSON.fromJObject <<< Array.foldl (\acc (Tuple k v) -> JO.insert k v acc) JO.empty

underIndex :: forall a. Int -> JArray -> (JSON -> Either String a) -> Either String a
underIndex i ja f = do
  a <- note ("missing required index " <> show i <> ".") $ JA.index ja i
  lmap (append ("under required index " <> show i <> ", ")) $ f a

underRequiredKey :: forall a. String -> JObject -> (JSON -> Either String a) -> Either String a
underRequiredKey k jo f = do
  a <- note ("missing required key " <> show k <> ".") $ JO.lookup k jo
  lmap (append ("under required key " <> show k <> ", ")) $ f a

withAttempts :: forall a j b. Array a -> (a -> j -> Either String b) -> j -> Either String b
withAttempts decoders fn j = decoders # flip Array.foldl (Left noDecodersFound) \acc a -> do
  case acc of
    Right _ -> acc
    Left e1
      | e1 == noDecodersFound -> fn a j
      | otherwise -> lmap (append (e1 <> "; next attempt: ")) $ fn a j
  where
  noDecodersFound = "No decoders found (array was empty)"

-- | All labels must have a function of type: `ToRecordCodec a`
-- | See `required`, `requiredRename`, `option`, `optionRename`.
toRecord
  :: forall codecs values codecsRL
   . RowToList codecs codecsRL
  => ToRecordObj codecsRL { | codecs } { | values }
  => { | codecs }
  -> JSON
  -> Either String { | values }
toRecord codecs = toJObject >=>
  toRecordObj (Proxy :: Proxy codecsRL) codecs

-- | Variant of `toRecord` that coerces the `Maybe { | r }` into a `Maybe NewtypedR`
toRecordN
  :: forall n codecs values codecsRL
   . RowToList codecs codecsRL
  => ToRecordObj codecsRL { | codecs } { | values }
  => Newtype n { | values }
  => ({ | values } -> n)
  -> { | codecs }
  -> JSON
  -> Either String n
toRecordN f codecs = coerce1 f <<< toRecord codecs

-- | Iterates through the underlying array.
-- | - key: uses the `str` in `Just str` or the record label if `Nothing`
-- | - f: the key used on the object and the result of looking up that key in the object
-- | - return: `Nothing` if the decoding failed; `Just` if it succeeded.
newtype ToRecordCodec a = ToRecordCodec (Either a (NonEmptyArray (Tuple (Maybe String) (String -> Maybe JSON -> Either String a))))

toStatic :: forall a. a -> ToRecordCodec a
toStatic a = ToRecordCodec $ Left a

toRequired :: forall a. (JSON -> Either String a) -> ToRecordCodec a
toRequired f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing $ \k j ->
  (note ("missing required key " <> show k <> ", ") j) >>= f >>> overKey k

toRequiredRename :: forall a. String -> (JSON -> Either String a) -> ToRecordCodec a
toRequiredRename jsonLbl f = ToRecordCodec $ Right $ NEA.singleton $ Tuple (Just jsonLbl) $ \k j ->
  (note ("missing required key " <> show k <> ", ") j) >>= f >>> overKey k

-- | Succeeds with Nothing if key wasn't found or with Just if key was found and value was succesfully decoded.
toOption :: forall a. (JSON -> Either String a) -> ToRecordCodec (Maybe a)
toOption f = toOptionDefault Nothing (map Just <$> f)

toOptionRename :: forall a. String -> (JSON -> Either String a) -> ToRecordCodec (Maybe a)
toOptionRename rename f = toOptionDefaultRename rename Nothing (map Just <$> f)

toOptionDefault :: forall a. a -> (JSON -> Either String a) -> ToRecordCodec a
toOptionDefault a f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing $ \k j ->
  maybe (Right a) (overKey k <<< f) j

toOptionDefaultRename :: forall a. String -> a -> (JSON -> Either String a) -> ToRecordCodec a
toOptionDefaultRename jsonLbl a f = ToRecordCodec $ Right $ NEA.singleton $ Tuple (Just jsonLbl) $ \k j ->
  maybe (Right a) (overKey k <<< f) j

toOptionArray :: forall a. (JSON -> Either String a) -> ToRecordCodec (Array a)
toOptionArray f = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \_ j -> case j of
  Nothing -> Right []
  Just j' -> toArray f j'

toOptionAssocArray :: forall a b. (String -> Either String a) -> (JSON -> Either String b) -> ToRecordCodec (Array (Tuple a b))
toOptionAssocArray k' v' = ToRecordCodec $ Right $ NEA.singleton $ Tuple Nothing \_ j -> case j of
  Nothing -> Right []
  Just j' -> (JO.toUnfoldable <$> toJObject j') >>= traverse (bitraverse k' v')

class ToRecordObj :: RowList Type -> Type -> Type -> Constraint
class ToRecordObj codecsRL codecs values | codecsRL -> codecs values where
  toRecordObj :: Proxy codecsRL -> codecs -> JObject -> Either String values

instance ToRecordObj RL.Nil {} {} where
  toRecordObj _ _ _ = Right {}

instance
  ( ToRecordObj codecTail { | cRest } { | vRest }
  , IsSymbol sym
  , Reflectable sym String
  , Row.Cons sym (ToRecordCodec a) cRest codecs
  , Row.Cons sym a vRest values
  , Row.Lacks sym vRest
  ) =>
  ToRecordObj (RL.Cons sym (ToRecordCodec a) codecTail) { | codecs } { | values } where
  toRecordObj _ codecs j = do
    rec <- toRecordObj (Proxy :: Proxy codecTail) codecsRest j
    a <- case keyDecoders of
      Left a' -> pure a'
      Right decs -> do
        j # withAttempts (NEA.toArray decs) \(Tuple keyRename decoder) j' -> do
          let key = fromMaybe lbl keyRename
          decoder key (JO.lookup key j')
    pure $ Record.insert _lbl a rec
    where
    lbl = reflectType _lbl
    _lbl = (Proxy :: Proxy sym)
    (ToRecordCodec keyDecoders) = Record.get _lbl codecs
    codecsRest = unsafeCoerce codecs
else instance
  ( Fail
      ( Above
          (Beside (Beside (Text "Expected 'ToRecordCodec a' for label '") (Text sym)) (Beside (Text "' but got type: ") (Quote a)))
          ( Above (Text "")
              (Text "User likely forgot to supply an additional argument or is not using `toRequired*`/`toOption*` variants.")
          )
      )
  ) =>
  ToRecordObj (RL.Cons sym a codecTail) { | codecs } { | values } where
  toRecordObj _ _ _ = unsafeCrashWith "Impossible"

newtype FromRecordCodec a = FromRecordCodec (Tuple (Maybe String) (String -> a -> Maybe JSON))

fromRequired :: forall a. (a -> JSON) -> FromRecordCodec a
fromRequired f = FromRecordCodec $ Tuple Nothing \_ -> Just <<< f

fromRequiredRename :: forall a. String -> (a -> JSON) -> FromRecordCodec a
fromRequiredRename str f = FromRecordCodec $ Tuple (Just str) \_ -> Just <<< f

-- | If Nothing, does not add the coressponding key
-- | If Just, adds the key and the encoded value to the JObject
fromOption :: forall a. (a -> JSON) -> FromRecordCodec (Maybe a)
fromOption f = FromRecordCodec $ Tuple Nothing \_ -> map f

fromOptionRename :: forall a. String -> (a -> JSON) -> FromRecordCodec (Maybe a)
fromOptionRename str f = FromRecordCodec $ Tuple (Just str) \_ -> map f

fromOptionArray :: forall a. (a -> JSON) -> FromRecordCodec (Array a)
fromOptionArray f = FromRecordCodec $ Tuple Nothing \_ arr ->
  if Array.length arr == 0 then Nothing
  else Just $ fromArray f arr

fromOptionAssocArray :: forall a b. (a -> String) -> (b -> JSON) -> FromRecordCodec (Array (Tuple a b))
fromOptionAssocArray k' v' = FromRecordCodec $ Tuple Nothing \_ arr ->
  if Array.length arr == 0 then Nothing
  else Just $ JSON.fromJObject $ Array.foldl (\acc (Tuple k v) -> JO.insert (k' k) (v' v) acc) JO.empty arr

-- | All labels must have a function of type: `FromRecordCodec a`
fromRecord
  :: forall codecs values codecsRL
   . RowToList codecs codecsRL
  => FromRecordObj codecsRL { | codecs } { | values }
  => { | codecs }
  -> { | values }
  -> JSON
fromRecord codecs values = JSON.fromJObject
  $ fromRecordObj (Proxy :: Proxy codecsRL) codecs values

fromRecordN
  :: forall n codecs values codecsRL
   . RowToList codecs codecsRL
  => FromRecordObj codecsRL { | codecs } { | values }
  => Newtype n { | values }
  => ({ | values } -> n)
  -> { | codecs }
  -> n
  -> JSON
fromRecordN _ codecs = unwrap >>> fromRecord codecs

class FromRecordObj :: RowList Type -> Type -> Type -> Constraint
class FromRecordObj codecsRL codecs values | codecsRL -> codecs values where
  fromRecordObj :: Proxy codecsRL -> codecs -> values -> JObject

instance FromRecordObj RL.Nil {} {} where
  fromRecordObj _ _ _ = JO.empty

instance
  ( FromRecordObj codecTail { | cRest } { | vRest }
  , IsSymbol sym
  , Reflectable sym String
  , Row.Cons sym (FromRecordCodec a) cRest codecs
  , Row.Cons sym a vRest values
  ) =>
  FromRecordObj (RL.Cons sym (FromRecordCodec a) codecTail) { | codecs } { | values } where
  fromRecordObj _ codecs values = do
    let obj = fromRecordObj (Proxy :: Proxy codecTail) cRest vRest
    let key = fromMaybe lbl keyRename
    case encoder key a' of
      Nothing -> obj
      Just a'' -> JO.insert key a'' obj
    where
    lbl = reflectType _lbl
    _lbl = (Proxy :: Proxy sym)
    (FromRecordCodec (Tuple keyRename encoder)) = Record.get _lbl codecs
    a' = Record.get _lbl values
    cRest = unsafeCoerce codecs
    vRest = unsafeCoerce values
else instance
  ( Fail
      ( Above
          (Beside (Beside (Text "Expected 'FromRecordCodec a' for label '") (Text sym)) (Beside (Text "' but got type: ") (Quote a)))
          ( Above (Text "")
              (Text "User likely forgot to supply an additional argument or is not using `fromRequired*`/`fromOption*` variants.")
          )
      )
  ) =>
  FromRecordObj (RL.Cons sym a codecTail) { | codecs } { | values } where
  fromRecordObj _ _ _ = unsafeCrashWith "Impossible"
