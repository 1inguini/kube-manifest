module Main where

import Manifest (yamls)
import Util (Yaml, YamlType (..), nonrootGid, nonrootOwn, nonrootUid, registry, rootGid, rootOwn, rootUid, s)
import qualified Util
import Util.Shake (
  dir,
  dirFile,
  gitClone,
  listDirectoryRecursive,
  mkdir,
  parallel_,
  runProg,
  tar,
  (<:>),
 )
import Util.Shake.Container (
  ImageName (ImageName),
  ImageRepo (ImageRepo),
  addContainerImageRule,
  docker,
  dockerCommit,
  dockerCopy,
  dockerPushEnd,
  image,
  latest,
  runDocker,
  withContainer,
 )
import qualified Util.Shake.Container as Image

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
import qualified Data.Char as Char
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
  Change (ChangeModtimeAndDigest),
  CmdOption (Cwd, InheritStdin),
  CmdResult,
  FilePattern,
  Lint (LintBasic),
  RuleResult,
  Rules,
  ShakeOptions (
    ShakeOptions,
    shakeChange,
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
  action,
  actionCatch,
  addOracleCache,
  addTarget,
  copyFile',
  getShakeOptions,
  getShakeOptionsRules,
  need,
  parallel,
  phony,
  phonys,
  produces,
  progressSimple,
  putError,
  putInfo,
  readFile',
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
import System.FilePath (
  addTrailingPathSeparator,
  dropExtension,
  dropTrailingPathSeparator,
  splitDirectories,
  takeDirectory,
  takeExtension,
  takeFileName,
  (</>),
 )
import System.Posix (
  getRealGroupID,
  getRealUserID,
  ownerExecuteMode,
  setFileCreationMask,
  setFileMode,
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

nonrootImage :: (?opts :: [CmdOption], ?shakeDir :: FilePath) => Rules ()
nonrootImage = do
  Image.registry "nonroot" `image` do
    ImageName (ImageRepo $ cs Util.registry </> "scratch", latest) `withContainer` [] $ do
      dockerCopy "nonroot/rootfs.tar" "/"
      dockerCommit ["ENTRYPOINT /bin/sh"]
      dockerPushEnd

  "nonroot/rootfs.tar" %> \out -> do
    need $ ("nonroot/rootfs/etc/" </>) <$> ["passwd", "group"]
    parallel_ $ mkdir . ("nonroot/rootfs" </>) <$> ["tmp", "home/nonroot"]
    tar nonrootOwn out

  writeFile' "nonroot/rootfs/etc/passwd" $
    unlines
      [ "root:x:0:0:root:/root:/sbin/nologin"
      , "nobody:x:65534:65534:Nobody:/nonexistent:/sbin/nologin"
      , "nonroot:x:" <> show nonrootUid <> ":" <> show nonrootGid <> ":nonroot:/home/nonroot:/sbin/nologin"
      ]

  writeFile' "nonroot/rootfs/etc/group" $
    unlines
      [ "root:x:0:"
      , "nobody:x:65534:"
      , "nonroot:x:" <> show nonrootGid <> ":"
      ]

archlinuxImage :: (?opts :: [CmdOption], ?shakeDir :: FilePath) => Rules ()
archlinuxImage = do
  Image.registry "archlinux" `image` do
    ImageName (Image.dockerIo "library/archlinux", latest) `withContainer` [] $ do
      let
        dockerExec :: [String] -> Action ()
        dockerExec = runDocker [] . (words "exec -i" <>)
        rootExec, nonrootExec :: [String] -> [String] -> Action ()
        rootExec opt = dockerExec . (opt <>) . (["--user=root", ?container] <>)
        nonrootExec opt = dockerExec . (opt <>) . (["--user=nonroot", ?container] <>)
      parallel_
        [ dockerCopy "archlinux/etc.tar" "/etc/"
        , dockerCopy "pacman/db/sync.tar" "/var/lib/pacman/sync"
        ]
      rootExec [] $ words "pacman --noconfirm -S git glibc moreutils rsync"
      parallel_
        [ do
            dockerCopy "archlinux/aur-helper.tar" "/home/nonroot/aur-helper/"
            nonrootExec ["--workdir=/home/nonroot/aur-helper"] ["makepkg", "--noconfirm", "-sir"]
        , rootExec [] ["locale-gen"]
        ]
      src <- fmap lines . readFile' $ "archlinux/aur-helper" </> dirFile
      current <- listDirectoryRecursive "archlinux/aur-helper"
      produces $ filter (`elem` (dirFile : src)) current
      dockerPushEnd

  -- "archlinux/aur-helper/" `dir` do
  --   gitClone "https://aur.archlinux.org/yay-bin.git" "master" ?dir

  "archlinux/aur-helper.tar" %> \out -> do
    need ["archlinux/aur-helper" </> dirFile]
    tar nonrootOwn out

  writeFile'
    "archlinux/etc/locale.gen"
    [str|en_US.UTF-8 UTF-8
        |ja_JP.UTF-8 UTF-8
        |]

  writeFile'
    "archlinux/etc/sudoers"
    [str|nonroot ALL=(ALL:ALL) NOPASSWD: ALL
        |]

  writeFile'
    "archlinux/etc/pacman.d/mirrorlist"
    [str|# Japan
        |Server = https://ftp.jaist.ac.jp/pub/Linux/ArchLinux/$repo/os/$arch
        |Server = https://mirrors.cat.net/archlinux/$repo/os/$arch
        |]

  "archlinux/etc.tar" %> \out -> do
    need . fmap ("archlinux/etc" </>) $
      [ "locale.gen"
      , "sudoers"
      , "pacman.d/mirrorlist"
      ]

pacRun ::
  (CmdResult r, ?projectRoot :: FilePath, ?shakeDir :: FilePath) =>
  [String] ->
  [CmdOption] ->
  [String] ->
  Action r
pacRun prog opts =
  runProg opts
    . (prog <>)
    . ( [ "--noconfirm"
        , "--config=" <> ?projectRoot </> "src/pacman.conf"
        , "--dbpath=" <> ?shakeDir </> "pacman"
        ]
          <>
      )

pacman, aur :: [String]
pacman = ["pacman"]
aur = ["yay", "--noprovides"]

pacmanSetup :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
pacmanSetup = do
  "pacman/sync/.tar" %> \out -> do
    need [?projectRoot </> "src/pacman.conf"]
    pacRun @() aur [] ["-Sy"]
    produces ["pacman/local/ALPM_DB_VERSION"]
    tar rootOwn out

dockerSetup :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
dockerSetup = do
  phony "docker/login" $
    runDocker [InheritStdin] ["login", head . splitDirectories . cs $ Util.registry]

musl :: (?opts :: [CmdOption], ?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
musl =
  "musl/lib/" `dir` do
    need ["pacman/db" </> dirFile]
    mkdir "musl/rootfs"
    pacRun @() aur [] ["--root=musl/rootfs", "musl"]
    runProg [] $ words "sudo mv musl/rootfs/usr/lib/musl/lib/* -t musl/lib"

skalibs :: (?opts :: [CmdOption], ?shakeDir :: FilePath) => Rules ()
skalibs =
  "skalibs/lib/" `dir` do
    let version = "v2.12.0.1"
    gitClone "https://github.com/skarnet/skalibs.git" version "skalibs/src"

    let cd = Cwd "skalibs/src"
    runProg @() [cd] $ words "./configure --disable-shared --libdir=../lib --sysdepdir=../sysdeps"
    let make = runProg @() [cd] . ("make" :) . (: [])
    make "all"
    make "strip"
    parallel_
      [ make "install-lib"
      , make "install-sysdeps"
      ]

s6PortableUtils :: (?opts :: [CmdOption], ?shakeDir :: FilePath) => Rules ()
s6PortableUtils = do
  let version = "v2.2.5.0"
  let s6 = "s6-portable-utils"

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
      let cd = Cwd (s6 </> "src")
      runProg @()
        [cd]
        [ "./configure"
        , "--bindir=" <> ?shakeDir </> takeDirectory out
        , "--enable-static-libc"
        , "--with-sysdeps=" <> ?shakeDir </> "skalibs/sysdeps"
        , "--with-libs=" <> ?shakeDir </> "skalibs/lib"
        , "--with-libs=" <> ?shakeDir </> "musl/lib"
        ]
      let make = runProg [cd] . ("make" :) . (: [])
      make "all"
      make "strip"
      make "install-bin"

busybox :: (?opts :: [CmdOption]) => Rules ()
busybox = download "busybox" "busybox" *> traverse_ singleApplet applets
 where
  version = "1.35.0"
  applets = ["cp"]
  download applet src =
    ("busybox" </> applet) %> \out -> do
      runProg @()
        []
        [ "curl"
        , "-L"
        , "https://busybox.net/downloads/binaries" </> version <> "-x86_64-linux-musl" </> src
        , "-o"
        , out
        ]
      liftIO $ setFileMode out 0o755
  singleApplet applet =
    download applet $ "busybox_" <> fmap Char.toUpper applet

rules :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
rules = do
  let ?opts = []
  addContainerImageRule

  pacmanSetup
  dockerSetup

  musl
  skalibs
  s6PortableUtils
  busybox

  nonrootImage
  archlinuxImage

main :: IO ()
main = do
  setFileCreationMask 0o022
  projectRoot <- liftIO $ makeAbsolute =<< getCurrentDirectory
  let ?projectRoot = projectRoot
      ?shakeDir = projectRoot </> shakeFiles shakeOptions
   in shakeArgsOptionsWith
        shakeOptions
          { shakeFiles = ?shakeDir
          , shakeThreads = 0
          , shakeLint = Just LintBasic
          , shakeColor = True
          , shakeProgress = progressSimple
          , shakeChange = ChangeModtimeAndDigest
          , shakeShare = Just ?shakeDir
          }
        []
        ( \case
            shakeOptions@ShakeOptions
              { shakeFiles
              , shakeReport
              , shakeLintInside
              , shakeLiveFiles
              , shakeShare
              } -> \_ targets -> do
                shakeFiles <- makeAbsolute shakeFiles
                shakeReport <- traverse makeAbsolute shakeReport
                shakeLintInside <- traverse makeAbsolute shakeLintInside
                shakeLiveFiles <- traverse makeAbsolute shakeLiveFiles
                shakeShare <-
                  Just <$> case shakeShare of
                    Nothing -> pure shakeFiles
                    Just shakeShare -> makeAbsolute shakeShare
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