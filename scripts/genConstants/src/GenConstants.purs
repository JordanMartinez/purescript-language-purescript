module Scripts.GenConstants where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Array as Array
import Data.List (List(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import GenConstants.Libs (codegenLibsModule)
import GenConstants.Prim (codegenPrimModule)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.Path as Path
import Node.FS.Perms as Perms
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Comment(..))
import Tidy.Codegen (leading, printModule)
import Tidy.Codegen.Monad (codegenModule)

main :: Effect Unit
main = launchAff_ do
  writeFile "Prim" $ unsafePartial codegenPrimModule
  writeFile "Lib" $ unsafePartial codegenLibsModule
  where
  moduleParts = [ "Language", "PureScript", "Constants" ]
  mkPath p = Path.concat $ append [ "src" ] $ Array.snoc moduleParts $ p <> ".purs"
  mkModuleName = Array.intercalate "." <<< Array.snoc moduleParts

  codegenHeader = map Comment
    [ "------------------------------------"
    , "-- This module is code generated. --"
    , "--          DO NOT EDIT!          --"
    , "------------------------------------"
    ]

  writeFile moduleName codegen = do
    let p = mkPath moduleName
    FSA.mkdir' (Path.dirname p) { recursive: true, mode: Perms.permsAll }
    FSA.writeTextFile UTF8 p
      $ printModule
      $ leading codegenHeader
      $ unsafePartial
      $ codegenModule (mkModuleName moduleName)
      $ runReaderT codegen { moduleName: "", prefix: "", varToDecs: Nil }
