module Main (
  main,
) where

import Manifest (yamls)
import Util (Yaml, YamlType (..), s)
import qualified Util

import Control.Applicative ((<|>))
import Control.Exception.Safe (Exception (displayException), finally, throw, throwString)
import Control.Monad (filterM, void, when)
import qualified Control.Monad.Catch as Exceptions (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (execState, get)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (AsValue (_Object, _String), key, _Key)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (foldlM, traverse_)
import Data.List (isPrefixOf)
import qualified Data.List as List
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (IsString (fromString))
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Texs
import qualified Data.Yaml as Yaml (decodeAllThrow, encode)
import Development.Shake (
  Action,
  FilePattern,
  Lint (LintBasic),
  RuleResult,
  Rules,
  ShakeOptions (
    ShakeOptions,
    shakeColor,
    shakeFiles,
    shakeLint,
    shakeLintInside,
    shakeLiveFiles,
    shakeProgress,
    shakeReport,
    shakeShare,
    shakeThreads
  ),
  actionCatch,
  addOracleCache,
  addTarget,
  copyFile',
  need,
  parallel,
  phony,
  phonys,
  produces,
  progressSimple,
  putError,
  shakeArgsOptionsWith,
  shakeOptions,
  want,
  withoutTargets,
  writeFile',
  (%>),
  (&%>),
 )
import qualified Development.Shake as Shake
import Development.Shake.Classes (Binary, Hashable, NFData, Typeable)
import Development.Shake.Rule (
  BuiltinIdentity,
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
import Optics (modifying, over, preview, view, (%), _head)
import System.Directory (
  createDirectoryIfMissing,
  getCurrentDirectory,
  listDirectory,
  makeAbsolute,
  removeDirectory,
  removeDirectoryRecursive,
  renameDirectory,
  renameFile,
  setCurrentDirectory,
 )
import qualified System.Directory as Sys (doesDirectoryExist, doesFileExist)
import System.FilePath (addTrailingPathSeparator, takeDirectory, (</>))
import System.Posix (CMode (CMode), setFileCreationMask)
import System.Posix.Files (setFileCreationMask)
import System.Process.Typed (
  ExitCode (ExitSuccess),
  proc,
  readProcessStdout,
  readProcessStdout_,
  readProcess_,
  runProcess_,
  setWorkingDir,
 )
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
--       let values = convertString $ Yaml.encode $ view #value yaml
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
instance Exceptions.MonadThrow Action where
  throwM err = do
    putError $ displayException err
    liftIO $ Exceptions.throwM err
instance Exceptions.MonadCatch Action where
  catch = actionCatch
instance Exceptions.MonadThrow Rules where
  throwM = liftIO . Exceptions.throwM

runProc :: MonadIO m => String -> m ()
runProc = runProcess_ . fromString

(<:>) :: String -> String -> String
x <:> y = x <> " " <> y

mkdir :: MonadIO m => FilePath -> m ()
mkdir = liftIO . createDirectoryIfMissing True

listDirectoryRecursive :: MonadIO m => FilePath -> m [FilePath]
listDirectoryRecursive dir = liftIO $ do
  fmap concat $
    traverse
      ( \path -> do
          let child = dir </> path
          isDir <- Sys.doesDirectoryExist child
          if isDir
            then fmap (path </>) <$> listDirectoryRecursive child
            else pure [path]
      )
      =<< listDirectory dir

producedDirectory :: FilePath -> Action ()
producedDirectory dir =
  produces . fmap (dir </>) =<< listDirectoryRecursive dir

dirFile :: String
dirFile = ".ls"

infix 1 `dir`
dir :: (?shakeDir :: FilePath) => FilePattern -> ((?dir :: FilePath) => Action ()) -> Rules ()
dir pat act = do
  phony (addTrailingPathSeparator pat) $ need [pat </> dirFile]
  withoutTargets $
    pat </> dirFile %> \out ->
      let ?dir = ?shakeDir </> takeDirectory out
       in do
            act
            ls <-
              filterM (\file -> (file /= dirFile &&) <$> liftIO (Sys.doesFileExist $ ?dir </> file))
                =<< listDirectoryRecursive ?dir
            produces $ (?dir </>) <$> ls
            writeFile' out $ unlines ls

parallel_ :: [Action a] -> Action ()
parallel_ = void . parallel

gitClone :: String -> String -> FilePath -> Action ()
gitClone repo tag dst = do
  mkdir dst
  liftIO $ removeDirectoryRecursive dst
  runProc $ "git clone --branch=" <> tag <:> repo <:> dst

docker = proc "podman"
aur opts =
  proc "yay" $
    opts
      <> [ "--config=" <> ?shakeDir </> "pacman/pacman.conf"
         , "--dbpath=" <> ?shakeDir </> "pacman/db"
         , "--noconfirm"
         , "--noprovides"
         ]
aurInstall opts = aur $ ["-S"] <> opts

newtype ImageRepo = ImageRepo {repo :: String}
  deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)
newtype ImageTag = ImageTag {tag :: String}
  deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)
newtype ImageName = ImageName (ImageRepo, ImageTag) -- (name, tag)
  deriving (Typeable, Eq, Hashable, Binary, NFData)
instance Show ImageName where
  show (ImageName (repo, tag)) = view #repo repo <> ":" <> view #tag tag
newtype Image = ImageId {id :: ByteString}
  deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)
newtype ImageRule = ImageRule {rule :: ImageName -> Maybe (Action ())}
type instance RuleResult ImageName = Image

addContainerImageRule :: Rules ()
addContainerImageRule = addBuiltinRule noLint imageIdentity run
 where
  imageIdentity :: BuiltinIdentity ImageName Image
  imageIdentity _ = Just . view #id

  newStore :: ImageName -> Action (Maybe ByteString)
  newStore name = do
    (exitCode, stdout) <-
      readProcessStdout . docker $
        words "images --no-trunc --quiet" <> [show name]
    case (exitCode, ByteString.split (fromIntegral $ fromEnum '\n') $ cs stdout) of
      (ExitSuccess, newStore : _) -> pure $ Just newStore
      _ -> pure Nothing

  run :: BuiltinRun ImageName Image
  run key oldStore RunDependenciesChanged = do
    (_, act) <- getUserRuleOne key (const Nothing) $ \(ImageRule act) -> act key
    act
    current <- newStore key
    case current of
      Nothing -> throwString $ "action did not create container image:" <:> show key
      Just current ->
        pure
          $ RunResult
            (if Just current == oldStore then ChangedRecomputeSame else ChangedRecomputeDiff)
            current
          $ ImageId current
  run key oldStore RunDependenciesSame = do
    current <- newStore key
    case current of
      Nothing -> run key oldStore RunDependenciesChanged
      Just current -> pure $ RunResult ChangedNothing current $ ImageId current

needImages :: [ImageName] -> Action ()
needImages = void . apply

needImage :: ImageName -> Action ()
needImage = void . apply1

type ContainerId = String
type ContainerfileCommand = String

latest :: ImageTag
latest = ImageTag "latest"

infix 1 `image`
image :: ImageRepo -> ((?imageName :: ImageName) => Action ()) -> Rules ()
image repo act = do
  addTarget $ view #repo repo
  addTarget . show $ ImageName (repo, latest)

  phonys $ \name -> do
    colonTag <- List.stripPrefix (view #repo repo) name
    let tag = case colonTag of
          "" -> Just latest
          ':' : tag -> Just $ ImageTag tag
          _ -> Nothing
    needImage . ImageName . (,) repo <$> tag

  addUserRule . ImageRule $ \case
    name@(ImageName (r, _)) | r == repo -> Just $ let ?imageName = name in act
    _ -> Nothing

addTaggedImageTarget :: ImageName -> Rules ()
addTaggedImageTarget = addTarget . show

nonrootImage :: (?shakeDir :: FilePath) => Rules ()
nonrootImage = do
  ImageRepo (cs Util.registry </> "nonroot") `image` do
    let pause = "s6-portable-utils/bin/s6-pause"
    need ["nonroot/rootfs.tar", pause]
    container <-
      fmap (init . cs) . readProcessStdout_ . docker . words $
        "run --detach --volume=" <> pause <> ":/pause --entrypoint=/pause" <:> cs Util.registry </> "scratch"
    let runDocker = runProcess_ . docker
    runDocker . words $ "cp nonroot.tar" <:> container <> ":/"
    runDocker ["commit", "--change", "ENTRYPOINT /bin/sh", container, show ?imageName]
    runDocker . words $ "stop --time=0" <:> container
    runDocker . words $ "rm" <:> container

  "nonroot/rootfs.tar" %> \out -> do
    need $ ("nonroot/rootfs/etc/" </>) <$> ["passwd", "group"]
    parallel_ $ mkdir . ("nonroot/rootfs" </>) <$> ["tmp", "home/nonroot"]
    runProc $ "tar -c --owner=nonroot --group=nonroot -f" <:> out <:> "-C nonroot/rootfs ."

  writeFile' "nonroot/rootfs/etc/passwd" $
    unlines
      [ "root:x:0:0:root:/root:/sbin/nologin"
      , "nobody:x:65534:65534:Nobody:/nonexistent:/sbin/nologin"
      , "nonroot:x:" <> show Util.nonroot <> ":" <> show Util.nonroot <> ":nonroot:/home/nonroot:/sbin/nologin"
      ]

  writeFile' "nonroot/rootfs/etc/group" $
    unlines
      [ "root:x:0:"
      , "nobody:x:65534:"
      , "nonroot:x:" <> show Util.nonroot <> ":"
      ]

pacmanSetup :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
pacmanSetup = do
  "pacman/pacman.conf" %> \out -> do
    copyFile' (?projectRoot </> "src/pacman.conf") out

  "pacman/db/" `dir` do
    need ["pacman/pacman.conf"]
    runProcess_ . aur . words $ "-Sy"

-- producedDirectory "pacman/db/sync"
-- writeFile' out mempty

musl :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
musl =
  "musl/lib/" `dir` do
    need ["pacman/db" </> dirFile]
    mkdir "musl/rootfs"
    runProcess_ $ aurInstall ["--root=musl/rootfs", "musl"]
    runProc $ "sudo mv musl/rootfs/usr/lib/musl/lib/*" <:> "-t musl/lib"

skalibs :: (?shakeDir :: FilePath) => Rules ()
skalibs =
  "skalibs/lib/" `dir` do
    let version = "v2.12.0.1"
    gitClone "https://github.com/skarnet/skalibs.git" version "skalibs/src"

    let run exe = runProcess_ . setWorkingDir "skalibs/src" . proc exe
    run "./configure" $ words "--disable-shared --libdir=../lib --sysdepdir=../sysdeps"
    run "make" ["all"]
    run "make" ["strip"]
    parallel_
      [ run "make" ["install-lib"]
      , run "make" ["install-sysdeps"]
      ]

s6PortableUtils :: (?shakeDir :: FilePath) => Rules ()
s6PortableUtils = do
  let version = "v2.2.5.0"
  let s6 = "s6-portable-utils"

  addTarget $ s6 </> "bin/*"
  withoutTargets $
    ((s6 </> "bin") </>)
      <$> [ "s6-basename"
          , "s6-cat"
          , "s6-chmod"
          , "s6-chown"
          , "s6-clock"
          , "s6-cut"
          , "s6-dirname"
          , "s6-dumpenv"
          , "s6-echo"
          , "s6-env"
          , "s6-expr"
          , "s6-false"
          , "s6-format-filter"
          , "s6-grep"
          , "s6-head"
          , "s6-hiercopy"
          , "s6-linkname"
          , "s6-ln"
          , "s6-ls"
          , "s6-maximumtime"
          , "s6-mkdir"
          , "s6-mkfifo"
          , "s6-nice"
          , "s6-nuke"
          , "s6-pause"
          , "s6-printenv"
          , "s6-quote"
          , "s6-quote-filter"
          , "s6-rename"
          , "s6-rmrf"
          , "s6-seq"
          , "s6-sleep"
          , "s6-sort"
          , "s6-sync"
          , "s6-tai64ndiff"
          , "s6-tail"
          , "s6-test"
          , "s6-touch"
          , "s6-true"
          , "s6-uniquename"
          , "s6-unquote"
          , "s6-unquote-filter"
          , "s6-update-symlinks"
          , "seekablepipe"
          ]
      &%> \outs@(out : _) -> do
        gitClone "https://github.com/skarnet/s6-portable-utils.git" version $ s6 </> "src"
        need ["musl/lib" </> dirFile, "skalibs/lib" </> dirFile]
        runProcess_ $
          setWorkingDir (s6 </> "src") $
            proc
              "./configure"
              [ "--bindir=" <> ?shakeDir </> takeDirectory out
              , "--enable-static-libc"
              , "--with-sysdeps=" <> ?shakeDir </> "skalibs/sysdeps"
              , "--with-libs=" <> ?shakeDir </> "skalibs/lib"
              , "--with-libs=" <> ?shakeDir </> "musl/lib"
              ]

        let make = runProcess_ . setWorkingDir (s6 </> "src") . proc "make" . (: [])
        make "all"
        make "strip"
        make "install-bin"

rules :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
rules = do
  addContainerImageRule

  pacmanSetup
  nonrootImage
  musl
  skalibs
  s6PortableUtils

main :: IO ()
main = do
  setFileCreationMask $ CMode 0o022
  projectRoot <- liftIO $ makeAbsolute =<< getCurrentDirectory
  let ?projectRoot = projectRoot
   in shakeArgsOptionsWith
        shakeOptions
          { shakeThreads = 0
          , shakeLint = Just LintBasic
          , shakeColor = True
          , shakeProgress = progressSimple
          }
        []
        ( \case
            shakeOptions@ShakeOptions
              { shakeFiles = shakeFiles'
              , shakeReport = shakeReport'
              , shakeLintInside = shakeLintInside'
              , shakeLiveFiles = shakeLiveFiles'
              , shakeShare = shakeShare'
              } -> \_ targets -> do
                shakeFiles <- makeAbsolute shakeFiles'
                shakeReport <- traverse makeAbsolute shakeReport'
                shakeLintInside <- traverse makeAbsolute shakeLintInside'
                shakeLiveFiles <- traverse makeAbsolute shakeLiveFiles'
                shakeShare <- fmap Just . makeAbsolute $ fromMaybe shakeFiles shakeShare'
                mkdir shakeFiles
                setCurrentDirectory shakeFiles
                pure $
                  Just
                    ( shakeOptions
                        { shakeFiles
                        , shakeReport
                        , shakeLintInside
                        , shakeLiveFiles
                        , shakeShare
                        }
                    , let ?shakeDir = shakeFiles in want targets *> rules
                    )
        )
        `finally` setCurrentDirectory ?projectRoot