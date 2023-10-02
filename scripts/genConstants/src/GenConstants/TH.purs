module GenConstants.TH where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Reader.Class (local)
import Control.Monad.Writer (tell)
import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.Identity (Identity)
import Data.List (List)
import Data.List as List
import Data.String (Pattern(..))
import Data.String as String
import Tidy.Codegen (declSignature, declValue, exprApp, exprCtor, exprIdent, exprString, typeApp, typeCtor)
import Tidy.Codegen.Monad (CodegenT, importCtor, importFrom, importType)

type Env =
  { moduleName :: String
  , prefix :: String
  , varToDecs :: List (String -> String -> String -> CodegenT Void (Free Identity) Unit)
  }

type AppM a = ReaderT Env (CodegenT Void (Free Identity)) a

mod
  :: Partial
  => String
  -> AppM Unit
  -> AppM Unit
mod name inner = do
  let parts = String.split (Pattern ".") name
  let valName = mkPrefixedName "m_" "" $ Array.intercalate "_" parts
  ReaderT \_ -> do
    mn <- importFrom "Language.PureScript.Names"
      { type: importType "ModuleName"
      , ctor: importCtor "ModuleName" "ModuleName"
      }
    tell
      [ declSignature valName (typeCtor mn.type)
      , declValue valName [] $ exprApp (exprCtor mn.ctor) [ exprString name ]
      ]
    pure unit
  local (const { moduleName: valName, prefix: "", varToDecs: List.Nil }) inner

cls
  :: Partial
  => String
  -> AppM Unit
cls cn = ReaderT \{ moduleName, prefix } -> do
  let valName = "cls" <> cap prefix <> cn
  names <- importFrom "Language.PureScript.Names"
    { tyProperName: importType "ProperName"
    , ctorProperName: importCtor "ProperName" "ProperName"
    , tyClassName: importType "ClassName"
    , tyQualified: importType "Qualified"
    , ctorQualified: importCtor "Qualified" "Qualified"
    , ctorByModuleName: importCtor "QualifiedBy" "ByModuleName"
    }
  tell
    [ declSignature valName $ typeApp (typeCtor names.tyQualified)
        [ typeApp (typeCtor names.tyProperName) [ typeCtor names.tyClassName ] ]
    , declValue valName [] $ exprApp (exprCtor names.ctorQualified)
        [ exprApp (exprCtor names.ctorByModuleName) [ exprIdent moduleName ]
        , exprApp (exprCtor names.ctorProperName) [ exprString cn ]
        ]
    ]

clss
  :: Partial
  => Array String
  -> AppM Unit
clss = traverse_ cls

dty
  :: Partial
  => String
  -> Array String
  -> AppM Unit
dty dn ctors = ReaderT \{ moduleName, prefix } -> do
  let dtName = "ty" <> cap prefix <> dn
  names <- importFrom "Language.PureScript.Names"
    { tyProperName: importType "ProperName"
    , ctorProperName: importCtor "ProperName" "ProperName"
    , tyTypeName: importType "TypeName"
    , tyConstructorName: importType "ConstructorName"
    , tyQualified: importType "Qualified"
    , ctorQualified: importCtor "Qualified" "Qualified"
    , ctorByModuleName: importCtor "QualifiedBy" "ByModuleName"
    }
  tell
    [ declSignature dtName $ typeApp (typeCtor names.tyQualified)
        [ typeApp (typeCtor names.tyProperName) [ typeCtor names.tyTypeName ] ]
    , declValue dtName [] $ exprApp (exprCtor names.ctorQualified)
        [ exprApp (exprCtor names.ctorByModuleName) [ exprIdent moduleName ]
        , exprApp (exprCtor names.ctorProperName) [ exprString dn ]
        ]
    ]
  for_ ctors \ctor -> do
    let ctorName = "ctor" <> cap prefix <> ctor
    tell
      [ declSignature ctorName $ typeApp (typeCtor names.tyQualified)
          [ typeApp (typeCtor names.tyProperName) [ typeCtor names.tyConstructorName ] ]
      , declValue ctorName [] $ exprApp (exprCtor names.ctorQualified)
          [ exprApp (exprCtor names.ctorByModuleName) [ exprIdent moduleName ]
          , exprApp (exprCtor names.ctorProperName) [ exprString ctor ]
          ]
      ]

nty
  :: Partial
  => String
  -> AppM Unit
nty tn = dty tn [ tn ]

ntys
  :: Partial
  => Array String
  -> AppM Unit
ntys = traverse_ nty

ty
  :: Partial
  => String
  -> AppM Unit
ty tn = ReaderT \{ moduleName, prefix } -> do
  let dtName = "ty" <> cap prefix <> tn
  names <- importFrom "Language.PureScript.Names"
    { tyProperName: importType "ProperName"
    , ctorProperName: importCtor "ProperName" "ProperName"
    , tyTypeName: importType "TypeName"
    , tyQualified: importType "Qualified"
    , ctorQualified: importCtor "Qualified" "Qualified"
    , ctorByModuleName: importCtor "QualifiedBy" "ByModuleName"
    }
  tell
    [ declSignature dtName $ typeApp (typeCtor names.tyQualified)
        [ typeApp (typeCtor names.tyProperName) [ typeCtor names.tyTypeName ] ]
    , declValue dtName [] $ exprApp (exprCtor names.ctorQualified)
        [ exprApp (exprCtor names.ctorByModuleName) [ exprIdent moduleName ]
        , exprApp (exprCtor names.ctorProperName) [ exprString tn ]
        ]
    ]

tys
  :: Partial
  => Array String
  -> AppM Unit
tys = traverse_ ty

var
  :: Partial
  => String
  -> AppM Unit
var nm = ReaderT \r -> do
  for_ r.varToDecs \f ->
    f r.moduleName r.prefix nm

vars
  :: Partial
  => Array String
  -> AppM Unit
vars = traverse_ var

asPair :: forall a. Partial => AppM a -> AppM a
asPair = local $ addToVars mkPair
  where
  mkPair moduleName prefix str = do
    let valName = mkPrefixedName "p_" prefix str
    tuple <- importFrom "Data.Tuple"
      { ty: importType "Tuple"
      , ctor: importCtor "Tuple" "Tuple"
      }
    names <- importFrom "Language.PureScript.Names"
      { tyModuleName: importType "ModuleName"
      }
    tell
      [ declSignature valName $ typeApp (typeCtor tuple.ty)
          [ typeCtor names.tyModuleName
          , typeCtor "String"
          ]
      , declValue valName [] $ exprApp (exprCtor tuple.ctor)
          [ exprIdent moduleName
          , exprString str
          ]
      ]

asIdent :: forall a. Partial => AppM a -> AppM a
asIdent = local (addToVars mkIdent)
  where
  mkIdent moduleName prefix str = do
    let valName = mkPrefixedName "i_" prefix str
    names <- importFrom "Language.PureScript.Names"
      { tyIdent: importType "Ident"
      , ctorIdent: importCtor "Ident" "Ident"
      , tyQualified: importType "Qualified"
      , ctorQualified: importCtor "Qualified" "Qualified"
      , ctorByModuleName: importCtor "QualifiedBy" "ByModuleName"
      }
    tell
      [ declSignature valName $ typeApp (typeCtor names.tyQualified)
          [ typeCtor names.tyIdent ]
      , declValue valName [] $ exprApp (exprCtor names.ctorQualified)
          [ exprApp (exprCtor names.ctorByModuleName) [ exprIdent moduleName ]
          , exprApp (exprCtor names.ctorIdent) [ exprString str ]
          ]
      ]

asString :: forall a. Partial => AppM a -> AppM a
asString = local $ addToVars mkString
  where
  mkString _ prefix str = do
    let valName = mkPrefixedName "s_" prefix str
    tell
      [ declSignature valName $ typeCtor "String"
      , declValue valName [] $ exprString str
      ]

addToVars :: (String -> String -> String -> CodegenT Void (Free Identity) Unit) -> Env -> Env
addToVars f r = r { varToDecs = f List.: r.varToDecs }

prefixWith :: forall a. String -> AppM a -> AppM a
prefixWith = local <<< applyPrefix

applyPrefix :: String -> Env -> Env
applyPrefix prefix r = r { prefix = camelAppend r.prefix prefix }

mkPrefixedName :: String -> String -> String -> String
mkPrefixedName tag prefix = append tag <<< camelAppend prefix

camelAppend :: String -> String -> String
camelAppend l r = if l == "" then r else l <> cap r

cap :: String -> String
cap s = (String.toUpper $ String.take 1 s) <> String.drop 1 s
