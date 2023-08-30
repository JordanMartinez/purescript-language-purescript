module Scripts.DownloadOldPackages where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Options ((:=))
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (for, traverse)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Foreign.Object as Object
import JSON (JSON)
import JSON as J
import JSON as JSON
import JSON.Object as JO
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.FS.Perms (permsAll)
import Node.FS.Sync as FS
import Node.HTTP.Client (RequestHeaders(..), headers, hostname, path, protocol, request, requestAsStream, responseAsStream, responseHeaders, statusCode, statusMessage)
import Node.Path (dirname)
import Node.Path as Path
import Node.Stream as Stream

main :: Effect Unit
main = launchAff_ do
  unlessM (liftEffect $ FS.exists fixturesDir) do
    FSA.mkdir fixturesDir
  unlessM (liftEffect $ FS.exists cacheFile) do
    FSA.writeTextFile UTF8 cacheFile "{}"
  cachedRepos <- getCachedRepos

  uncachedDownloadUrls <- map Map.fromFoldable $ for repos \repo -> do
    urls <- getVersionUrls repo
    case urls of
      Nothing -> do
        Console.log $ "No URLs. Returning empty set for repo"
        pure $ Tuple repo Set.empty
      Just urls' ->
        case Map.lookup repo cachedRepos of
          Nothing -> do
            Console.log "Nothing was cached."
            pure $ Tuple repo urls'
          Just cachedUrls -> do
            let neededUrls = Set.difference urls' cachedUrls
            Console.log $ "Something was cached, but still need " <> show (Set.size neededUrls) <> " files."
            pure $ Tuple repo neededUrls

  newlyCachedRepos <- forWithIndex uncachedDownloadUrls \repo urls -> do
    Console.log $ "Getting versions for repo: " <> repo <> " (count: " <> show (Set.size urls) <> ")"
    results <- for (Set.toUnfoldable urls) \info@(Tuple version downloadUrl) -> do
      Console.log $ "Getting doc file for " <> repo <> "@" <> version <> " [" <> downloadUrl <> "]"
      content <- getUrl $ Download downloadUrl
      case content of
        Left msg -> do
          Console.error $ "Failed to get docs.json for " <> repo <> "@" <> version <> ": " <> msg
          pure Nothing
        Right txt -> do
          let docsPath = Path.concat [ fixturesDir, repo, version ]
          unlessM (liftEffect $ FS.exists $ dirname docsPath) do
            FSA.mkdir' (dirname docsPath) { recursive: true, mode: permsAll }
          FSA.writeTextFile UTF8 docsPath txt
          pure $ Just info
    pure $ Set.fromFoldable $ Array.catMaybes results

  Console.log $ "newly cached repos: " <> show newlyCachedRepos

  writeCache $ Map.unionWith Set.union cachedRepos newlyCachedRepos
  where
  fixturesDir = Path.concat [ "language-purescript-types", "test", "fixtures" ]
  cacheFile = Path.concat [ fixturesDir, "cache.json" ]

  getCachedRepos :: Aff (Map String (Set (Tuple String String)))
  getCachedRepos = do
    cache <- FSA.readTextFile UTF8 cacheFile
    case J.parse cache of
      Left e -> do
        Console.error $ "cache failed to parse " <> e
        pure Map.empty
      Right j -> case decodeRepoVersions j of
        Nothing -> do
          Console.error "cache failed to decode"
          pure Map.empty
        Just a -> do
          Console.log "cache recovered"
          pure a

  writeCache :: Map String (Set (Tuple String String)) -> Aff Unit
  writeCache = FSA.writeTextFile UTF8 cacheFile <<< J.printIndented <<< encodeRepoVersions

  getVersionUrls :: String -> Aff (Maybe (Set (Tuple String String)))
  getVersionUrls repo = do
    content <- getUrl $ Api $ "/repos/purescript/pursuit-backups/contents/purescript-" <> repo
    case content of
      Left err -> do
        Console.error $ "For repo " <> repo <> " - request error: " <> err
        pure Nothing
      Right c ->
        case J.parse c of
          Left e -> do
            Console.error $ "For repo " <> repo <> " - JSON parse error: " <> e
            pure Nothing
          Right j -> do
            case decodeLibVersions j of
              Nothing -> do
                Console.error $ "For repo " <> repo <> " - json decode error"
                pure Nothing
              Just a -> do
                Console.log $ "For repo " <> repo <> " - success"
                Console.log $ show a
                pure $ Just a

decodeLibVersions :: JSON -> Maybe (Set (Tuple String String))
decodeLibVersions = J.toArray >=> traverse convert >>> map Set.fromFoldable
  where
  convert j = do
    obj <- J.toJObject j
    name <- JO.lookup "name" obj >>= J.toString
    downloadUrl <- JO.lookup "download_url" obj >>= J.toString
    pure $ Tuple name downloadUrl

encodeRepoVersions :: Map String (Set (Tuple String String)) -> JSON
encodeRepoVersions = Map.toUnfoldable >>> map (map Set.toUnfoldable) >>> encodeArrayAsObj (encodeArrayAsObj JSON.fromString)

encodeArrayAsObj :: forall a. (a -> JSON) -> Array (Tuple String a) -> JSON
encodeArrayAsObj encodeA = JSON.fromJObject <<< flip Array.foldl JO.empty \acc (Tuple k v) ->
  JO.insert k (encodeA v) acc

decodeRepoVersions :: JSON -> Maybe (Map String (Set (Tuple String String)))
decodeRepoVersions = decodeObjAsArray (decodeObjAsArray JSON.toString) >>> map ((map (map Set.fromFoldable)) >>> (Map.fromFoldable :: Array _ -> _))

decodeObjAsArray :: forall a. (JSON -> Maybe a) -> JSON -> Maybe (Array (Tuple String a))
decodeObjAsArray decodeA = JSON.toJObject >>> map JO.toUnfoldable >=> traverse (traverse decodeA)

repos :: Array String
repos =
  [ "prelude"
  , "arrays"
  , "maybe"
  , "foldable-traversable"
  ]

data UrlType
  = Api String
  | Download String

getUrl :: UrlType -> Aff (Either String String)
getUrl urlType = do
  let
    { isApi, host, url } = case urlType of
      Api url -> { isApi: true, host: "api.github.com", url }
      Download url -> { isApi: false, host: "raw.githubusercontent.com", url: fromMaybe url $ String.stripPrefix (Pattern "https://raw.githubusercontent.com") url }
  log $ "Getting file for URL: " <> url
  let
    options =
      Array.fold $
        [ protocol := "https:"
        , hostname := host
        , path := url
        ] <>
          ( if isApi then
              [ headers :=
                  ( RequestHeaders $ Object.fromFoldable
                      [ Tuple "User-Agent" "language-purescript-types-script-for-tests"
                      , Tuple "Accept" "application/vnd.github+jso"
                      , Tuple "X-GitHub-Api-Version" "2022-11-28"
                      ]
                  )
              ]
            else []
          )
  resp <- makeAff \done -> do
    req <- request options (done <<< Right)
    Stream.end (requestAsStream req) mempty
    pure nonCanceler
  content <- makeAff \done -> do
    let strm = responseAsStream resp
    buffs <- Ref.new []
    Stream.onData strm \buf ->
      Ref.modify_ (flip Array.snoc buf) buffs
    Stream.onEnd strm do
      bufs <- Ref.read buffs
      buf <- Buffer.concat bufs :: Effect _
      str <- Buffer.toString UTF8 buf :: Effect _
      done $ Right str
    pure $ nonCanceler
  case statusCode resp of
    200 -> do
      pure $ Right content
    _ -> do
      pure $ Left $ Array.intercalate "\n"
        [ "URL error for " <> url
        , "status code: " <> show (statusCode resp)
        , "status msg: " <> statusMessage resp
        , "headers: " <> show (responseHeaders resp)
        , "content: "
        , content
        ]
