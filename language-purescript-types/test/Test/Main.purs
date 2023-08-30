module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..))
import Data.Traversable (for_)
import Data.Version (version)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import JSON as JSON
import Language.PureScript.Docs.Types (jsonDModule, jsonGithubUser, jsonPackage)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.FS.Stats as Stat
import Node.FS.Sync as FSSync
import Node.Path as Path

main :: Effect Unit
main = launchAff_ do
  runDependencyDocsDecodeTest
  runOldCompilerPackageTest

runDependencyDocsDecodeTest :: Aff Unit
runDependencyDocsDecodeTest = do
  children <- FSA.readdir output
  for_ children \child -> do
    let path = Path.concat [ output, child, "docs.json" ]
    exists <- liftEffect $ FSSync.exists path
    when exists do
      errOrJson <- map JSON.parse $ FSA.readTextFile UTF8 path
      case errOrJson of
        Left e ->
          Console.error $ "JSON parse error - path " <> child <> ": " <> e
        Right a -> do
          case jsonDModule a of
            Left e ->
              Console.error $ "JSON decode error - path " <> child <> ". Error:\n\t" <> e
            Right _ ->
              Console.log $ "Successfully decoded path: " <> child
  where
  output = "output"

runOldCompilerPackageTest :: Aff Unit
runOldCompilerPackageTest = do
  repos <- FSA.readdir fixturesDir
  for_ repos \repo -> do
    let repoDir = Path.concat [ fixturesDir, repo ]
    s <- FSA.stat repoDir
    when (Stat.isDirectory s) do
      versions <- FSA.readdir repoDir
      for_ versions \versionFile -> do
        let docsFile = Path.concat [ repoDir, versionFile ]
        errOrJson <- map JSON.parse $ FSA.readTextFile UTF8 docsFile
        case errOrJson of
          Left e ->
            Console.error $ "JSON parse error - " <> repo <> "@" <> versionFile <> ": " <> e
          Right a -> do
            case jsonPackage (version 0 7 0 Nil Nil) jsonGithubUser a of
              Left e ->
                Console.error $ "JSON decode error - " <> repo <> "@" <> versionFile <> " Error:\n\t" <> e
              Right _ ->
                Console.log $ "Successfully decoded path: " <> repo <> "@" <> versionFile
  where
  fixturesDir = Path.concat [ "language-purescript-types", "test", "fixtures" ]
