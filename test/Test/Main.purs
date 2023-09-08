module Test.Main where

import Prelude

import Codec.Json.Unidirectional.Value as Json
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Traversable (for_)
import Data.Version (version)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Language.PureScript.Docs.Types (toDocModule, toGithubUser, toPackage)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.FS.Stats as Stat
import Node.FS.Sync as FSSync
import Node.Path (FilePath)
import Node.Path as Path

main :: Effect Unit
main = launchAff_ do
  runDependencyDocsDecodeTest
  runOldCompilerPackageTest
  runPursuitTestsPackageTest

runDependencyDocsDecodeTest :: Aff Unit
runDependencyDocsDecodeTest = do
  children <- FSA.readdir output
  for_ children \child -> do
    let path = Path.concat [ output, child, "docs.json" ]
    exists <- liftEffect $ FSSync.exists path
    when exists do
      decodeFile toDocModule path child
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
        let
          docsFile = Path.concat [ repoDir, versionFile ]
          codec = toPackage (version 0 7 0 Nil Nil) toGithubUser
        decodeFile codec docsFile $ repo <> "@" <> versionFile

  where
  fixturesDir = Path.concat [ "test", "fixtures" ]

runPursuitTestsPackageTest :: Aff Unit
runPursuitTestsPackageTest = do
  compilerVersions <- FSA.readdir jsonCompatDir
  for_ compilerVersions \compilerVersion -> do
    let compilerDir = Path.concat [ jsonCompatDir, compilerVersion ]
    s <- FSA.stat compilerDir
    when (Stat.isDirectory s) do
      versions <- FSA.readdir compilerDir
      for_ versions \versionFile -> do
        let
          docsFile = Path.concat [ compilerDir, versionFile ]
          codec = toPackage (version 0 7 0 Nil Nil) toGithubUser
        decodeFile codec docsFile $ "(compiler: " <> compilerDir <> "; repo-version: " <> versionFile <> ")"

  where
  jsonCompatDir = Path.concat [ "test", "json-compat" ]

decodeFile :: forall a. (Json -> Either Json.DecodeError a) -> FilePath -> String -> Aff Unit
decodeFile codec file ref = do
  fromerrOr <- map jsonParser $ FSA.readTextFile UTF8 file
  case fromerrOr of
    Left e ->
      Console.error $ "JSON parse error - " <> ref <> ": " <> e
    Right a -> do
      case codec a of
        Left e ->
          Console.error $ "JSON decode error - " <> ref <> " Error:\n" <> Json.printDecodeError e
        Right _ ->
          Console.log $ "Successfully decoded path: " <> ref
