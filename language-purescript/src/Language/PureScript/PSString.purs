module Language.PureScript.PSString
  ( PSString
  , toUTF16CodeUnits
  , mkPSString
  , fromPsString
  , toPSString
  , decodeString
  , decodeStringEither
  , decodeStringWithReplacement
  , prettyPrintStringPS
  , prettyPrintStringJS
  ) where

import Prelude

import Codec.Json.Unidirectional.Value as Json
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Char (toCharCode)
import Data.CodePoint.Unicode (generalCategory)
import Data.CodePoint.Unicode as GeneralCategory
import Data.CodeUnit (CodeUnit, fromCodeUnit, decodeUtf16BE, decodeUtf16BEStr, encodeUtf16BE, toCodeUnit, prettyPrintCodeUnitJS, prettyPrintCodeUnitPS, showHex', unpairBE)
import Data.Either (Either, either)
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Data.String (CodePoint, toCodePointArray)
import Data.String as SCP
import Data.String.CodeUnits as SCU
import Partial.Unsafe (unsafePartial)

-- | An array of UTF-16BE code units
newtype PSString = PSString (Array CodeUnit)

toUTF16CodeUnits :: PSString -> Array CodeUnit
toUTF16CodeUnits (PSString x) = x

derive newtype instance Eq PSString
derive newtype instance Ord PSString
derive newtype instance Semigroup PSString
derive newtype instance Monoid PSString

instance Show PSString where
  show = show <<< codePoints

fromPsString :: PSString -> Json
fromPsString psStr = case decodeString psStr of
  Just str -> Json.fromString str
  Nothing -> Json.fromArray fromCodeUnit $ toUTF16CodeUnits psStr

toPSString :: Json -> Either Json.DecodeError PSString

toPSString j = asString <|> asCodeUnitArray
  where
  asString = mkPSString <$> Json.toString j
  asCodeUnitArray = PSString <$> Json.toArray toCodeUnit j

mkPSString :: String -> PSString
mkPSString = PSString <<< Array.concatMap encodeUtf16BE <<< toCodePointArray

decodeString :: PSString -> Maybe String
decodeString = toMaybe <<< decodeUtf16BEImpl <<< Array.concatMap unpairBE <<< toUTF16CodeUnits

codePoints :: PSString -> String
codePoints = toUTF16CodeUnits >>>
  decodeUtf16BEStr (SCU.singleton <<< unsafePartial fromJust <<< toEnum <<< fromEnum)

decodeStringEither :: PSString -> Array (Either CodeUnit CodePoint)
decodeStringEither = decodeUtf16BE <<< toUTF16CodeUnits

decodeStringWithReplacement :: PSString -> String
decodeStringWithReplacement = toUTF16CodeUnits >>>
  decodeUtf16BEStr (const "\xFFFD")

foreign import decodeUtf16BEImpl :: Array Int -> Nullable String

prettyPrintStringPS :: PSString -> String
prettyPrintStringPS s = "\"" <> Array.foldMap (either prettyPrintCodeUnitPS encodeCodePoint) (decodeStringEither s) <> "\""
  where
  encodeCodePoint cp = encode $ fromEnum cp
    where
    encode c
      | c == toCharCode '\t' = "\\t"
      | c == toCharCode '\r' = "\\r"
      | c == toCharCode '\n' = "\\n"
      | c == toCharCode '"' = "\\\""
      | c == toCharCode '\'' = "\\\'"
      | c == toCharCode '\\' = "\\\\"
      | shouldPrint cp = SCP.singleton cp
      | otherwise = showHex' "\\x" 6 c

  shouldPrint :: CodePoint -> Boolean
  shouldPrint cp
    | i <- fromEnum cp, i == toCharCode ' ' = true
    | otherwise = fromMaybe false do
        cat <- generalCategory cp
        pure $ Array.elem cat
          [ GeneralCategory.UppercaseLetter
          , GeneralCategory.LowercaseLetter
          , GeneralCategory.TitlecaseLetter
          , GeneralCategory.OtherLetter
          , GeneralCategory.DecimalNumber
          , GeneralCategory.LetterNumber
          , GeneralCategory.OtherNumber
          , GeneralCategory.ConnectorPunctuation
          , GeneralCategory.DashPunctuation
          , GeneralCategory.OpenPunctuation
          , GeneralCategory.ClosePunctuation
          , GeneralCategory.InitialQuote
          , GeneralCategory.FinalQuote
          , GeneralCategory.OtherPunctuation
          , GeneralCategory.MathSymbol
          , GeneralCategory.CurrencySymbol
          , GeneralCategory.ModifierSymbol
          , GeneralCategory.OtherSymbol
          ]

prettyPrintStringJS :: PSString -> String
prettyPrintStringJS s = "\"" <> Array.foldMap prettyPrintCodeUnitJS (toUTF16CodeUnits s) <> "\""
