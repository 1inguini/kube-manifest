module Main (
  main,
) where

import Manifest (yamls)
import Util (Yaml, YamlType (..), tshow)
import qualified Util

-- import Control.Applicative ((<|>))
-- import Control.Monad (void)
-- import Control.Monad.State.Strict (execState, get)
-- import qualified Data.Aeson.KeyMap as KeyMap
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString as ByteString
-- import Data.Foldable (foldlM)
-- import Data.Maybe (catMaybes, fromMaybe)
-- import Data.String (IsString (fromString))
-- import qualified Data.Yaml as Yaml (decodeAllThrow, encode)
-- import System.Directory (createDirectoryIfMissing)
-- import System.Process (readProcess)

import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry (Entry (entryOwnership), Ownership (Ownership, ownerName))
import qualified Codec.Archive.Tar.Entry as Tar
import Control.Exception.Safe (throwString)
import Control.Lens ((^.))
import Control.Monad (unless, void)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import qualified Data.Aeson as Aeson
import Data.Aeson.Optics (AsValue (_Object, _String), key, _Key)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable (hashWithSalt), hash)
import qualified Data.List as List
import Data.Record.Anon
import Data.Record.Anon.Simple (Record)
import qualified Data.Record.Anon.Simple as Anon
import Data.String.Conversions (ConvertibleStrings, cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.These (These (That, These, This))
import Data.Time (UTCTime, ZonedTime, getCurrentTime, getZonedTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Development.Shake (
  Lint (LintBasic),
  addOracle,
  addOracleCache,
  addTarget,
  askOracle,
  getShakeOptionsRules,
  phony,
  phonys,
  progressSimple,
 )
import Development.Shake.Command (CmdArgument, IsCmdArgument (toCmdArgument))
import Development.Shake.Plus (fromRelDir, parseRelDir)
import Development.Shake.Plus hiding (addOracle, addOracleCache, askOracle, phony, (%>))
import qualified Development.Shake.Plus as Shake ((%>))
import Development.Shake.Rule (
  BuiltinRun,
  RunChanged (ChangedNothing, ChangedRecomputeDiff, ChangedRecomputeSame),
  RunMode (RunDependenciesChanged, RunDependenciesSame),
  RunResult (RunResult),
  addBuiltinRule,
  addUserRule,
  apply,
  apply1,
  getUserRuleOne,
  noLint,
 )
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Network.Wreq as Wreq (get, responseBody)
import Optics (modifying, over, preview, set, view, (%), _head)
import Path.IO (createDir, createDirIfMissing, doesPathExist)
import qualified Secret as Util
import Text.Heredoc (here)

-- processYaml :: [FilePath] -> Yaml -> IO [FilePath]
-- processYaml written yaml =
--   case view #yamlType yaml of
--     Manifest ->
--       let ?namespace =
--             fromMaybe Util.noNamespace $
--               preview (#value % key "metadata" % key "namespace" % _String) yaml <|> do
--                 kind <- preview (#value % key "kind" % _String) yaml
--                 if kind == "Namespace"
--                   then preview (#value % key "metadata" % key "name" % _String) yaml
--                   else Nothing
--           ?app = fromMaybe "unknown-app" $ preview (#value % key "metadata" % key "labels" % key "app" % _String) yaml
--        in objectWrite written $ view #value yaml
--     HelmValues r -> do
--       let ?namespace = view #namespace r
--       let values = cs $ Yaml.encode $ view #value yaml
--       aesons <-
--         readProcess
--           "helm"
--           [ "template"
--           , "--values"
--           , "-"
--           , view #chart r
--           ]
--           values
--           >>= Yaml.decodeAllThrow @IO @Aeson.Value . fromString
--       let objects =
--             execState
--               ( do
--                   modifying (key "metadata" % _Object) $
--                     KeyMap.insert "namespace" $
--                       Aeson.String ?namespace
--                   yaml <- get
--                   modifying (key "metadata" % key "labels" % _Object) $
--                     KeyMap.insert "app" $
--                       fromMaybe "unknown-app" $
--                         preview
--                           (key "metadata" % key "labels" % key (view (#appLabel % _Key) r))
--                           yaml
--               )
--               <$> aesons
--       let ?app =
--             fromMaybe "unknown-app" $
--               preview (_head % key "metadata" % key "labels" % key "app" % _String) objects
--       valuesPath <- path "values/"
--       putStrLn $ "# writing to: " <> valuesPath
--       ByteString.writeFile valuesPath $ Yaml.encode $ view #value yaml
--       written <- processYaml written $ Util.manifest Util.namespace
--       foldlM objectWrite written objects
--  where
--   path dir = do
--     createDirectoryIfMissing True dir
--     pure $ dir <> Text.unpack ?app <> ".yaml"
--   objectWrite :: (?namespace :: Text, ?app :: Text) => [FilePath] -> Aeson.Value -> IO [FilePath]
--   objectWrite written object = do
--     path <-
--       path $
--         "manifest/"
--           <> Text.unpack ?namespace
--           <> "/"
--           <> ( let neverDelete = "never-delete/"
--                 in case preview (key "kind") object of
--                     Just "PersistentVolumeClaim" -> neverDelete
--                     Just "Namespace" -> neverDelete
--                     _ -> mempty
--              )
--     putStrLn $ "# writing to: " <> path
--     ( if path `elem` written
--         then ByteString.appendFile path . ("---\n" <>)
--         else ByteString.writeFile path
--       )
--       $ Yaml.encode object
--     pure $ path : written

-- generate :: IO ()
-- generate = void $ foldlM processYaml [] yamls

-- main :: IO ()
-- main = generate

s :: String -> String
s = id

-- tar
fileEntry :: MonadThrow m => Path Rel File -> ByteString -> m Tar.Entry
fileEntry path content = do
  path <- either throwString pure . Tar.toTarPath False . cs $ toFilePath path
  pure (Tar.fileEntry path $ cs content){entryOwnership = nonrootOwn}

nonrootOwn :: Ownership
nonrootOwn =
  Ownership
    { ownerName = "nonroot"
    , groupName = "nonroot"
    , ownerId = Util.nonroot
    , groupId = Util.nonroot
    }

directoryEntry :: MonadThrow m => Path Rel Dir -> m Tar.Entry
directoryEntry path = do
  path <- either throwString pure . Tar.toTarPath True . cs $ toFilePath path
  pure (Tar.directoryEntry path){entryOwnership = nonrootOwn}

instance MonadThrow Action where
  throwM = liftIO . throwM

instance MonadThrow Rules where
  throwM = liftIO . throwM
instance CmdResult r => CmdArguments (RAction e r) where
  cmdArguments (CmdArgument x) = case partitionEithers x of
    (opts, x : xs) -> liftAction $ command opts x xs
    _ -> error "Error, no executable or arguments given to Development.Shake.cmd"

instance IsCmdArgument Text where
  toCmdArgument = toCmdArgument . (cs :: Text -> String)

instance IsCmdArgument [Text] where
  toCmdArgument = toCmdArgument . fmap (cs :: Text -> String)

instance IsCmdArgument ByteString where
  toCmdArgument = toCmdArgument . (cs :: ByteString -> String)

instance IsCmdArgument [ByteString] where
  toCmdArgument = toCmdArgument . fmap (cs :: ByteString -> String)

infixl 4 %>
(%>) ::
  (HasCallStack, MonadRules m) =>
  Path Rel File ->
  (Path Rel File -> Action ()) ->
  m ()
(%>) file act = runShakePlus () $ toFilePath file Shake.%> (liftAction . act)

docker :: CmdArgument
docker = cmd $ s "podman"
from :: Image -> Image -> [String] -> (ContainerId -> Action ()) -> Action ()
from base image changes act = do
  Stdout container <- cmd docker (s "container create") (imageName base) (s "sh")
  act $ ContainerId $ filter (not . isSpace) container
  labels <- labels image
  commit image (labels <> changes) $ ContainerId container
copy :: [Tar.Entry] -> ContainerId -> Action ()
copy entries (ContainerId container) = cmd_ (StdinBS (Tar.write entries)) docker (s "cp -") $ container <> ":/"
commit :: Image -> [String] -> ContainerId -> Action ()
commit image changes (ContainerId container) = do
  cmd_ docker (s "commit") (concatMap (\x -> ["--change", x]) changes) container $ imageName image
labels :: Image -> Action [String]
labels image = do
  dateTime <- askUTCTime
  pure
    [ "LABEL org.opencontainers.image.created=" <> dateTime
    , "LABEL org.opencontainers.image.authors=1inguini <9647142@gmail.com>"
    , "LABEL org.opencontainers.image.url=" <> cs (imageName image)
    , "LABEL org.opencontainers.image.documentation=https://git.1inguini.com/1inguini/kube-manifest/README.md"
    , "LABEL org.opencontainers.image.source=https://git.1inguini.com/1inguini/kube-manifest"
    ]
description :: String -> String
description = ("LABEL org.opencontainers.image.description=" <>)

-- https://stackoverflow.com/q/54050016
newtype Tag = Tag String
  deriving (Show, Eq, Hashable, Binary, NFData)

latest :: Tag
latest = Tag "latest"

newtype Image = Image (Path Rel File, Tag) -- repo/name, tag
  deriving (Show, Eq, Hashable, Binary, NFData)

imageName (Image (name, Tag tag)) =
  toFilePath name <> ":" <> tag

registry :: Path Rel Dir
registry = $(TH.lift =<< TH.runIO ((</> [reldir|library|]) <$> parseRelDir ("registry." <> cs Util.host)))

scratch :: Image
scratch = Image (registry </> [relfile|scratch|], latest)

newtype ImageHash = ImageHash ByteString
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult Image = ImageHash -- image id (sha256)

newtype ImageRule = ImageRule (Image -> Maybe (Action ()))

needImages :: MonadAction m => [Image] -> m ()
needImages = liftAction . void . apply

needImage :: MonadAction m => Image -> m ()
needImage = liftAction . void . apply1

newtype ContainerId = ContainerId String
  deriving (Show, Eq, Hashable, Binary, NFData)

singleLine :: (MonadThrow m, ConvertibleStrings String s) => String -> m s
singleLine str = case lines str of
  [line] -> pure $ cs line
  lines -> throwString $ "multiple corresponding Images" <> show lines

addContainerImageRule :: Rules ()
addContainerImageRule = do
  liftRules $ addBuiltinRule noLint imageIdentity run
 where
  imageIdentity _ (ImageHash hash) = Just hash

  imageSha image = do
    Stdout out <- cmd docker (s "image list --no-trunc --quiet") $ imageName image
    singleLine out

  run :: BuiltinRun Image ImageHash
  run key oldStore RunDependenciesChanged = do
    (_, act) <- getUserRuleOne key (const Nothing) $ \(ImageRule act) -> act key
    act
    current <- imageSha key
    pure
      $ RunResult
        (if Just current == oldStore then ChangedRecomputeSame else ChangedRecomputeDiff)
        current
      $ ImageHash current
  run key oldStore RunDependenciesSame = do
    current <- imageSha key
    pure
      $ RunResult
        (if Just current == oldStore then ChangedNothing else ChangedRecomputeDiff)
        current
      $ ImageHash current

imageRule :: Image -> (Image -> Action ()) -> Rules ()
imageRule image action = do
  liftRules $ addUserRule $ ImageRule (\i -> if image == i then Just $ action i else Nothing)
  phony (imageName image) $ needImage image

imageRuleTagless :: Path Rel File -> (Image -> Action ()) -> Rules ()
imageRuleTagless name act = do
  liftRules . addUserRule . ImageRule $ \case
    image@(Image (n, _)) | n == name -> Just $ act image
    _ -> Nothing

  phonys $ \image -> do
    tag <- List.stripPrefix (toFilePath name <> ":") image
    pure $ needImage $ Image (name, Tag tag)

newtype ListDynamicDep = ListDynamicDep () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ListDynamicDep = [Path Abs File]

newtype Download = Download String deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult Download = ByteString

addDownloadRule :: Rules ()
addDownloadRule = void . addOracleCache $ \(Download url) ->
  liftIO $ ByteString.toStrict . (^. Wreq.responseBody) <$> Wreq.get url

needDownload :: String -> Action ByteString
needDownload = askOracle . Download

newtype ISO8601DateTime = ISO8601DateTime () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ISO8601DateTime = String

addUTCTimeRule :: Rules ()
addUTCTimeRule = void . addOracle $ \(ISO8601DateTime ()) -> liftIO $ iso8601Show <$> getCurrentTime

askUTCTime :: MonadAction m => m String
askUTCTime = liftAction . askOracle $ ISO8601DateTime ()

instance Binary (Path Rel t)
instance Binary Tar.Entry where
  put entry = put $ Tar.write [entry]
  get = do
    entries <- Tar.read <$> get
    case entries of
      Tar.Next entry _ -> pure entry
      e -> fail $ show e
instance Hashable Tar.Entry where
  hashWithSalt salt entry = hashWithSalt salt $ Tar.write [entry]
newtype TarEntry t c = TarEntry (Path Rel t, c) deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult (TarEntry File ByteString) = Tar.Entry
type instance RuleResult (TarEntry Dir ()) = Tar.Entry

addTarEntryRule :: Rules ()
addTarEntryRule = do
  void . addOracleCache $ \(TarEntry (path, content)) -> fileEntry path content
  void . addOracleCache $ \(TarEntry (path, ())) -> directoryEntry path

needFileEntry :: Path Rel File -> ByteString -> Action Tar.Entry
needFileEntry path = askOracle . curry TarEntry path

needDirEntry :: Path Rel Dir -> Action Tar.Entry
needDirEntry path = askOracle $ TarEntry (path, ())

main :: IO ()
main = shakeArgs
  shakeOptions
    { shakeThreads = 0
    , shakeLint = Just LintBasic
    , shakeColor = True
    , shakeProgress = progressSimple
    }
  $ do
    let artifactDir = [reldir|artifact|]
    shakeOptions <- liftRules getShakeOptionsRules
    shakeDir <- parseRelDir $ shakeFiles shakeOptions
    let buildDir = shakeDir </> [reldir|build|]
    let image = buildDir </> [reldir|image|]
    let rootfs = buildDir </> [reldir|rootfs|]

    addDownloadRule
    addUTCTimeRule
    addTarEntryRule
    addContainerImageRule

    -- rootfs </> [relfile|passwd|] %> \out -> do
    --   let passwd =
    --         fileEntry "etc/passwd" $
    --           ByteString.unlines
    --             [ "root:x:0:0:root:/root:/sbin/nologin"
    --             , "nonroot:x:" <> tshow nonroot <> ":" <> tshow nonroot <> ":nonroot:/home/nonroot:/sbin/nologin"
    --             ]
    --   pure ()

    -- rootfs </> [relfile|group|] %> \out ->
    --   writeFile' out $
    --     Text.unlines
    --       [ "root:x:0:"
    --       , "nonroot:x:" <> tshow nonroot
    --       ]

    -- cacert <-
    --   askDownload
    --     "https://ccadb-public.secure.force.com/mozilla/IncludedRootsPEMTxt?TrustBitsInclude=Websites"

    imageRuleTagless (registry </> [relfile|nonroot|]) $ \image@(Image (name, _)) -> do
      let ruleDir = buildDir </> name

      tar <-
        parallel
          [ needDirEntry [reldir|tmp|]
          , needDirEntry [reldir|home/nonroot|]
          , needFileEntry [relfile|etc/passwd|]
              . cs
              $ Text.unlines
                [ "root:x:0:0:root:/root:/sbin/nologin"
                , "nonroot:x:" <> tshow Util.nonroot <> ":" <> tshow Util.nonroot <> ":nonroot:/home/nonroot:/sbin/nologin"
                ]
          , needFileEntry [relfile|etc/group|] . cs $
              Text.unlines ["root:x:0:", "nonroot:x:" <> tshow Util.nonroot]
          ]
      from
        scratch
        image
        [ "WORKDIR /home/nonroot"
        , "USER nonroot"
        , description "scratch with nonroot user"
        ]
        $ copy tar

      pure ()