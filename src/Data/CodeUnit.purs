module Data.CodeUnit where

import Prelude

import Codec.Json.Unidirectional.Value as Json
import Control.Monad.ST.Internal (while)
import Control.Monad.ST.Internal as ST
import Control.Monad.ST.Internal as STRef
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Array.ST as STA
import Data.Char (toCharCode)
import Data.Either (Either(..), note)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultPred, defaultSucc, fromEnum, toEnum)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Int.Bits (shr, (.&.))
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid (power)
import Data.String (CodePoint)
import Data.String as SCP
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

newtype CodeUnit = CodeUnit Int

derive instance Eq CodeUnit
derive instance Ord CodeUnit
derive newtype instance Show CodeUnit
instance Enum CodeUnit where
  succ = defaultSucc toEnum fromEnum
  pred = defaultPred toEnum fromEnum

instance Bounded CodeUnit where
  bottom = CodeUnit 0
  top = CodeUnit 65535

instance BoundedEnum CodeUnit where
  cardinality = Cardinality 65535
  toEnum c
    | between 0 65535 c = Just $ CodeUnit c
    | otherwise = Nothing
  fromEnum (CodeUnit c) = c

fromCodeUnit :: CodeUnit -> Json
fromCodeUnit = fromEnum >>> Json.fromInt

toCodeUnit :: Json -> Either Json.DecodeError CodeUnit
toCodeUnit = Json.toInt >=> toEnum >>> note (Json.DecodeError "Value out of bounds for CodeUnit (0 <= x <= 65535)")

unpairBE :: CodeUnit -> Array Int
unpairBE c = [ highByte c, lowByte c ]

unpairLE :: CodeUnit -> Array Int
unpairLE c = [ lowByte c, highByte c ]

-- https://stackoverflow.com/a/53567998
lowByte :: CodeUnit -> Int
lowByte (CodeUnit c) = c .&. 0xFF

-- https://stackoverflow.com/a/53567998
highByte :: CodeUnit -> Int
highByte (CodeUnit c) = (shr c 8) .&. 0xFF

isLead :: Int -> Boolean
isLead h = h >= 0xD800 && h <= 0xDBFF

isTrail :: Int -> Boolean
isTrail l = l >= 0xDC00 && l <= 0xDFFF

isSurrogate :: Int -> Boolean
isSurrogate c = isLead c || isTrail c

decodeUtf16BE :: Array CodeUnit -> Array (Either CodeUnit CodePoint)
decodeUtf16BE cus = ST.run do
  let len = Array.length cus
  let lastIdx = len - 1
  arr <- STA.new
  currentIdx <- STRef.new 0

  while ((notEq len) <$> STRef.read currentIdx) do
    idx <- STRef.read currentIdx
    let h'@(CodeUnit h) = unsafePartial $ Array.unsafeIndex cus idx
    if idx + 1 <= lastIdx then do
      let (CodeUnit l) = unsafePartial $ Array.unsafeIndex cus (idx + 1)
      if isLead h && isTrail l then do
        _ <- STA.push (Right $ unsafePartial $ fromJust $ toEnum $ (h - 0xD800) * 0x400 + (l - 0xDC00) + 0x10000) arr
        STRef.write (idx + 2) currentIdx
      else do
        _ <- STA.push (if isSurrogate h then Left h' else Right $ unsafePartial $ fromJust $ toEnum h) arr
        STRef.write (idx + 1) currentIdx
    else do
      _ <- STA.push (if isSurrogate h then Left h' else Right $ unsafePartial $ fromJust $ toEnum h) arr
      STRef.write (idx + 1) currentIdx

  STA.unsafeFreeze arr

decodeUtf16BEStr :: (CodeUnit -> String) -> Array CodeUnit -> String
decodeUtf16BEStr codeUnitToStr cus = ST.run do
  let len = Array.length cus
  let lastIdx = len - 1
  str <- STRef.new ""
  currentIdx <- STRef.new 0

  while ((notEq len) <$> STRef.read currentIdx) do
    idx <- STRef.read currentIdx
    let CodeUnit h = unsafePartial $ Array.unsafeIndex cus idx
    if idx + 1 <= lastIdx then do
      let (CodeUnit l) = unsafePartial $ Array.unsafeIndex cus (idx + 1)
      if isLead h && isTrail l then do
        _ <- str # STRef.modify \s -> append s
          $ SCP.singleton
          $ unsafePartial
          $ fromJust
          $ toEnum
          $ (h - 0xD800) * 0x400 + (l - 0xDC00) + 0x10000
        STRef.write (idx + 2) currentIdx
      else do
        _ <- str # STRef.modify \s -> append s
          if isSurrogate h then codeUnitToStr $ unsafePartial $ fromJust $ toEnum h
          else SCP.singleton $ unsafePartial $ fromJust $ toEnum h
        STRef.write (idx + 1) currentIdx
    else do
      _ <- str # STRef.modify \s -> append s
        if isSurrogate h then codeUnitToStr $ unsafePartial $ fromJust $ toEnum h
        else SCP.singleton $ unsafePartial $ fromJust $ toEnum h
      STRef.write (idx + 1) currentIdx

  STRef.read str

encodeUtf16BE ∷ CodePoint → Array CodeUnit
encodeUtf16BE cp = do
  let cpAsInt = fromEnum cp
  if cpAsInt >= 0xFFFF then do
    let Tuple high low = surrogatesBE cpAsInt
    [ high, low ]
  else do
    [ CodeUnit cpAsInt ]
  where
  surrogatesBE :: Int -> Tuple CodeUnit CodeUnit
  surrogatesBE cpAsInt = Tuple (CodeUnit $ h + 0xD800) (CodeUnit $ l + 0xDC00)
    where
    Tuple h l = divMod (cpAsInt - 0x10000) 0x400

divMod :: Int -> Int -> Tuple Int Int
divMod l r = Tuple (l / r) (l `mod` r)

prettyPrintCodeUnitPS :: CodeUnit -> String
prettyPrintCodeUnitPS = showHex' "\\x" 6 <<< fromEnum

prettyPrintCodeUnitJS :: CodeUnit -> String
prettyPrintCodeUnitJS (CodeUnit c)
  | c > 0xFF = showHex' "\\u" 4 c
  | c > 0x7E || c < 0x20 = showHex' "\\x" 2 c
  | c == 0x0008 {- '\b' -} = "\\b"
  | c == toCharCode '\t' = "\\t"
  | c == toCharCode '\n' = "\\n"
  | c == 0x000b {- '\v' -} = "\\v"
  | c == 0x000c {- '\f' -} = "\\f"
  | c == toCharCode '\r' = "\\r"
  | c == toCharCode '"' = "\\\""
  | c == toCharCode '\\' = "\\\\"
  | otherwise = SCU.singleton $ unsafePartial fromJust $ toEnum c

showHex' :: String -> Int -> Int -> String
showHex' prefix width c = do
  let hs = Int.toStringAs hexadecimal c
  prefix <> power "0" (width - String.length hs) <> hs
