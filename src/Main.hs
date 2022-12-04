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
import Control.Exception.Safe (handle, throwString)
import Control.Lens ((^.))
import Control.Monad (foldM, unless, void, when)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import qualified Data.Aeson as Aeson
import Data.Aeson.Optics (AsValue (_Object, _String), key, _Key)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Foldable (concatMap, traverse_)
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
  actionCatch,
  addOracle,
  addOracleCache,
  addTarget,
  getShakeOptionsRules,
  phony,
  phonys,
  progressSimple,
 )
import Development.Shake.Command (CmdArgument (CmdArgument), IsCmdArgument (toCmdArgument))
import Development.Shake.Plus hiding (CmdOption (Env), addOracle, addOracleCache, phony, (%>))
import qualified Development.Shake.Plus as Shake (CmdOption (Env), (%>))
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
import Path.IO (createDir, createDirIfMissing, doesPathExist, ensureDir)
import qualified Secret as Util
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Text.Heredoc (here, str)

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
instance MonadThrow Action where
  throwM = liftIO . throwM

instance MonadCatch Action where
  catch = actionCatch

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
  Path b File ->
  (forall b. Path b File -> Action ()) ->
  m ()
(%>) file act = runShakePlus () $ toFilePath file Shake.%> (liftAction . act)

data ImageConfig
  = Cmd [String]
  | Entrypoint [String]
  | Env String String
  | Expose String
  | Label String String
  | User String
  | Volume String
  | Workdir String
  deriving (Show, Eq, Generic)

toChange :: ImageConfig -> String
toChange (Cmd cmds) = "CMD " <> show cmds
toChange (Entrypoint cmds) = "ENTRYPOINT " <> show cmds
toChange (Env var val) = "ENV " <> var <> "=" <> val
toChange (Expose port) = "EXPOSE " <> port
toChange (Label label val) = "LABEL " <> label <> "=" <> val
toChange (User user) = "USER " <> user
toChange (Volume dir) = "VOLUME " <> dir
toChange (Workdir dir) = "WORKDIR " <> dir

toArgs :: ImageConfig -> [String]
toArgs (Cmd cmds) = "--cmd" : cmds
toArgs (Entrypoint cmds) = "--entrypoint" : cmds
toArgs (Env var val) = ["--env", var, val]
toArgs (Expose port) = ["--port", port]
toArgs (Label label val) = ["--label", label <> "=" <> val]
toArgs (User user) = ["--user", user]
toArgs (Volume dir) = ["--volume", dir]
toArgs (Workdir dir) = ["--workingdir", dir]

buildah :: CmdArgument
buildah = cmd $ s "buildah"
from :: Image -> Image -> RAction ContainerId () -> Action ()
from image base act = do
  Stdout container <- cmd buildah (s "from") $ imageName base
  container <- ContainerId <$> singleLine container
  labels <- labels image
  runRAction container $
    act *> commit image
copy :: Path b Dir -> RAction ContainerId ()
copy path = do
  ContainerId container <- ask
  cmd_ buildah [s "copy", "--chown=" <> show Util.nonroot, "--chmod=755"] container (toFilePath path) (s "/")
runBy :: CmdResult r => String -> CmdArgument -> RAction ContainerId r
runBy user (CmdArgument args) = do
  ContainerId container <- ask
  let (opts, commands) = partitionEithers args
  let (buildahOpts, execOpts) =
        foldl
          ( \(buildahOpts, execOpts) ->
              \case
                (Cwd cwd) -> (buildahOpts, ("--workdir=" <> cwd) : execOpts)
                opt -> (opt : buildahOpts, execOpts)
          )
          ([], [])
          opts
  cmd buildahOpts buildah ("run --user=" <> user) execOpts container commands
run, rootRun :: CmdResult r => CmdArgument -> RAction ContainerId r
run = runBy "nonroot"
rootRun = runBy "root"
run_, rootRun_ :: CmdArgument -> RAction ContainerId ()
run_ = run
rootRun_ = rootRun
config :: [ImageConfig] -> RAction ContainerId ()
config config = do
  ContainerId container <- ask
  cmd_ buildah (s "config") (concatMap toArgs config) container
commit :: Image -> RAction ContainerId ()
commit image = do
  ContainerId container <- ask
  cmd_ buildah (s "commit --rm") container $ imageName image
labels :: MonadAction m => Image -> m [ImageConfig]
labels image = do
  dateTime <- getUTCTime
  pure $
    (\(l, v) -> Label ("org.opencontainers.image." <> l) v)
      <$> [ ("created", dateTime)
          , ("authors", "1inguini <9647142@gmail.com>")
          , ("url", cs (imageName image))
          , ("documentation", "https://git.1inguini.com/1inguini/kube-manifest/README.md")
          , ("source", "https://git.1inguini.com/1inguini/kube-manifest")
          ]

description :: String -> ImageConfig
description = Label "org.opencontainers.image.description"

pacman, aur :: CmdArgument
aur = cmd $ s "yay --noconfirm --noprovides"
pacman = cmd $ s "pacman --noconfirm" :: CmdArgument

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

registryImage :: Path Rel File -> Tag -> Image
registryImage name = curry Image (registry </> name)
scratch, nonroot :: Image
scratch = registryImage [relfile|scratch|] latest
nonroot = registryImage [relfile|nonroot|] latest

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
  [] -> throwString "no lines"
  lines -> throwString $ "multiple lines" <> show lines

addContainerImageRule :: Rules ()
addContainerImageRule = do
  liftRules $ addBuiltinRule noLint imageIdentity run
 where
  imageIdentity _ (ImageHash hash) = Just hash

  imageSha image = do
    (Stdout out, Exit code) <- cmd buildah (s "images --no-trunc --quiet") $ imageName image
    case code of
      ExitSuccess -> singleLine out
      ExitFailure 125 -> pure ""

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
    if ByteString.null current
      then run key oldStore RunDependenciesChanged
      else pure $ RunResult ChangedNothing current $ ImageHash current

infix 4 `imageRuleFrom`
infix 4 `imageRuleArbitaryTagsFrom`

imageRuleFrom :: Image -> Image -> RAction ContainerId () -> Rules ()
imageRuleFrom image@(Image (name, tag)) base action = do
  phony (imageName image) $ needImage image
  when (tag == latest) $ phony (toFilePath name) $ needImage image
  liftRules $
    addUserRule $
      ImageRule
        ( \i ->
            if image == i
              then Just $ (image `from` base) action
              else Nothing
        )

imageRuleArbitaryTagsFrom :: Path Rel File -> Image -> [ImageConfig] -> RAction ContainerId () -> Rules ()
imageRuleArbitaryTagsFrom name base confs act = do
  phonys $ \image -> do
    colTag <- List.stripPrefix (toFilePath name) image
    tag <- case colTag of
      "" -> pure "latest"
      ':' : tag -> pure tag
      _ -> Nothing
    pure $ needImage $ Image (name, Tag tag)

  liftRules . addUserRule . ImageRule $ \case
    image@(Image (n, _)) | n == name -> Just $ (image `from` base) act
    _ -> Nothing

newtype ListDynamicDep = ListDynamicDep () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ListDynamicDep = [Path Abs File]

newtype Download = Download String deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult Download = ByteString

addDownloadRule :: Rules ()
addDownloadRule = void . addOracleCache $ \(Download url) ->
  liftIO $ ByteString.toStrict . (^. Wreq.responseBody) <$> Wreq.get url

needDownload :: MonadAction m => String -> m ByteString
needDownload = askOracle . Download

getUTCTime :: MonadAction m => m String
getUTCTime = liftIO $ iso8601Show <$> getCurrentTime

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

nonrootOwn :: Ownership
nonrootOwn =
  Ownership
    { ownerName = "nonroot"
    , groupName = "nonroot"
    , ownerId = Util.nonroot
    , groupId = Util.nonroot
    }

-- tar
fileEntry :: MonadThrow m => Path Rel File -> ByteString -> m Tar.Entry
fileEntry path content = do
  path <- either throwString pure . Tar.toTarPath False . cs $ toFilePath path
  pure (Tar.fileEntry path $ cs content){entryOwnership = nonrootOwn}

directoryEntry :: MonadThrow m => Path Rel Dir -> m Tar.Entry
directoryEntry path = do
  path <- either throwString pure . Tar.toTarPath True . cs $ toFilePath path
  pure (Tar.directoryEntry path){entryOwnership = nonrootOwn}

addTarEntryRule :: Rules ()
addTarEntryRule = do
  void . addOracleCache $ \(TarEntry (path, content)) -> fileEntry path content
  void . addOracleCache $ \(TarEntry (path, ())) -> directoryEntry path

needFileEntry :: MonadAction m => Path Rel File -> ByteString -> m Tar.Entry
needFileEntry path = askOracle . curry TarEntry path

needDirEntry :: MonadAction m => Path Rel Dir -> m Tar.Entry
needDirEntry path = askOracle $ TarEntry (path, ())

buildDir :: (?shakeDir :: Path a Dir) => Path a Dir
buildDir = ?shakeDir </> [reldir|build|]

writeFileRule :: Path a File -> Text -> Rules ()
writeFileRule path content = path %> flip writeFile' content
writeFileLinesRule :: Path a File -> [Text] -> Rules ()
writeFileLinesRule path content = path %> flip writeFileLines content
writeFileInRule :: (?dir :: Path a Dir) => Path Rel File -> Text -> Rules ()
writeFileInRule path = writeFileRule (?dir </> path)
writeFileLinesInRule :: (?dir :: Path a Dir) => Path Rel File -> [Text] -> Rules ()
writeFileLinesInRule path = writeFileLinesRule (?dir </> path)

imageRules :: (?shakeDir :: Path b Dir) => Rules ()
imageRules = do
  let ?workDir = buildDir </> [reldir|image/nonroot|]
  nonrootImageRules

nonrootImageRules :: (?workDir :: Path b Dir) => Rules ()
nonrootImageRules =
  do
    let ?workDir = ?workDir </> [reldir|nonroot|]
    let ?dir = ?workDir
     in do
          writeFileLinesInRule
            [relfile|etc/passwd|]
            [ "root:x:0:0:root:/root:/sbin/nologin"
            , "nobody:x:65534:65534:Nobody:/nonexistent:/sbin/nologin"
            , "nonroot:x:" <> tshow Util.nonroot <> ":" <> tshow Util.nonroot <> ":nonroot:/home/nonroot:/sbin/nologin"
            ]
          writeFileLinesInRule
            [relfile|etc/group|]
            [ "root:x:0:"
            , "nobody:x:65534:"
            , "nonroot:x:" <> tshow Util.nonroot <> ":"
            ]

    (nonroot `imageRuleFrom` scratch) $
      do
        needIn
          (?workDir </> [reldir|etc|])
          [ [relfile|passwd|]
          , [relfile|group|]
          ]

        traverse_
          (ensureDir . (?workDir </>))
          [ [reldir|tmp|]
          , [reldir|home/nonroot|]
          ]

        config
          [ Workdir "/home/nonroot"
          , User "nonroot"
          , description "scratch with nonroot user"
          ]
        copy ?workDir

main :: IO ()
main = shakeArgs
  shakeOptions
    { shakeThreads = 0
    , shakeLint = Just LintBasic
    , shakeColor = True
    , shakeProgress = progressSimple
    }
  $ do
    shakeDir <- parseRelDir . shakeFiles =<< getShakeOptionsRules
    let ?shakeDir = shakeDir
        ?artifactDir = [reldir|artifact|]
     in do
          addDownloadRule
          addTarEntryRule
          addContainerImageRule

          imageRules

-- (registry </> [relfile|archlinux|] `imageRuleArbitaryTagsFrom` Image ([relfile|docker.io/library/archlinux|], Tag "base-devel"))
--   [ Workdir "/home/nonroot"
--   , User "nonroot"
--   , description "Arch Linux with nonroot user and aur helper"
--   ]
--   $ do
--     let nonroot = show Util.nonroot
--     execWith $ \Exec{rootExec_} ->
--       void $
--         parallel
--           [ do
--               rootExec_ $ cmd (s "groupadd --gid") nonroot (s "nonroot")
--               rootExec_ $ cmd (s "useradd --uid") nonroot (s "--gid") nonroot (s "-m -s /usr/bin/nologin nonroot")
--           , rootExec_ $
--               cmd
--                 (Stdin "nonroot ALL=(ALL:ALL) NOPASSWD: ALL")
--                 (s "tee -a /etc/sudoers")
--           , let noExtract =
--                   [str|NoExtract  = etc/systemd/*
--                       |NoExtract  = usr/share/systemd/*
--                       |NoExtract  = usr/share/man/*
--                       |NoExtract  = usr/share/help/*
--                       |NoExtract  = usr/share/doc/*
--                       |NoExtract  = usr/share/gtk-doc/*
--                       |NoExtract  = usr/share/info/*
--                       |NoExtract  = usr/share/X11/*
--                       |NoExtract  = usr/share/systemd/*
--                       |NoExtract  = usr/share/bash-completion/*
--                       |NoExtract  = usr/share/fish/*
--                       |NoExtract  = usr/share/zsh/*
--                       |NoExtract  = usr/lib/systemd/*
--                       |NoExtract  = usr/lib/sysusers.d/*
--                       |NoExtract  = usr/lib/tmpfiles.d/*
--                       |]
--              in do
--                   rootExec_ $ cmd (s "sed -i /etc/pacman.conf -e") [s "/^NoExtract/d"]
--                   rootExec_ $ cmd (Stdin noExtract) (s "tee -a /etc/pacman.conf")
--           ]
--     mirrorlist <- cs <$> readFile' [relfile|container/builder/mirrorlist|]
--     copy . (: []) =<< needFileEntry [relfile|etc/pacman.d/mirrorlist|] mirrorlist

--     execWith $ \Exec{exec_, rootExec_} -> do
--       rootExec_ $ cmd pacman (s "-Sy git moreutils rsync")
--       exec_ $
--         cmd
--           (Cwd "/home/nonroot")
--           (s "git clone https://aur.archlinux.org/yay-bin.git aur-helper")
--       exec_ $ cmd (Cwd "/home/nonroot/aur-helper") (s "makepkg --noconfirm -sir")
--       rootExec_ $ cmd pacman (s "-S glibc")
--     copy . (: [])
--       =<< needFileEntry
--         [relfile|etc/locale.gen|]
--         (cs $ Text.unlines ["en_US.UTF-8 UTF-8", "ja_JP.UTF-8 UTF-8"])
--     execWith $ \Exec{rootExec_} -> rootExec_ . cmd $ s "locale-gen"
