module Main where

import Manifest (yamls)
import Util (Yaml, YamlType (..), nonrootGid, nonrootUid, registry, s)
import qualified Util
import Util.Shake (
  aur,
  aurInstall,
  dir,
  dirFile,
  gitClone,
  mkdir,
  pacman,
  parallel_,
  runProc,
  (<:>),
 )
import Util.Shake.Container (
  ImageName (ImageName),
  ImageRepo (ImageRepo),
  addContainerImageRule,
  docker,
  dockerRunPause,
  image,
  latest,
  runDocker,
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
import System.FilePath (addTrailingPathSeparator, dropTrailingPathSeparator, takeDirectory, (</>))
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

nonrootImage :: (?shakeDir :: FilePath) => Rules ()
nonrootImage = do
  let ?proc = proc
  ImageRepo (cs Util.registry </> "nonroot") `image` do
    let pause = "s6-portable-utils/bin/s6-pause"
    need ["docker/login", "nonroot/rootfs.tar", pause]
    container <- dockerRunPause $ ImageName (ImageRepo $ cs Util.registry </> "scratch", latest)
    let runDocker = runProcess_ . docker
    runDocker . words $ "cp nonroot/rootfs.tar" <:> container <> ":/"
    runDocker ["commit", "--change", "ENTRYPOINT /bin/sh", container, show ?imageName]

    parallel_
      [ runDocker ["push", show ?imageName]
      , do
          runDocker . words $ "stop --time=0" <:> container
          runDocker . words $ "rm" <:> container
      ]

  "nonroot/rootfs.tar" %> \out -> do
    need $ ("nonroot/rootfs/etc/" </>) <$> ["passwd", "group"]
    parallel_ $ mkdir . ("nonroot/rootfs" </>) <$> ["tmp", "home/nonroot"]
    runProc $ "tar -c --owner=nonroot --group=nonroot -f" <:> out <:> "-C nonroot/rootfs ."

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

archlinuxImage :: (?shakeDir :: FilePath) => Rules ()
archlinuxImage = do
  let ?proc = proc
  ImageRepo (cs Util.registry </> "archlinux") `image` do
    need ["docker/login", "archlinux/rootfs.tar"]
    container <- dockerRunPause $ ImageName (Image.registry "library/archlinux", latest)
    let
      dockerExec :: [String] -> Action ()
      dockerExec = runDocker . (words "exec -i" <>)
      rootExec, nonrootExec :: String -> [String] -> Action ()
      rootExec command = dockerExec . ("--user=root" :) . (command :)
      nonrootExec command = dockerExec . ("--user=nonroot" :) . (command :)
    parallel_
      [ do
          rootExec "groupadd" ["--gid=" <> show nonrootGid, "nonroot"]
          rootExec "useradd" $
            ["--uid=" <> show nonrootUid, "--gid=" <> show nonrootGid]
              <> words "-m -s /usr/bin/nologin nonroot"
      , runDocker $ "copy" : ["archlinux/etc.tar", container <> ":/etc"]
      ]
    let ?proc = rootExec
     in pacman $ words "-Sy git glibc moreutils rsync"

  writeFile'
    "archlinux/etc/locale.gen"
    [str|en_US.UTF-8 UTF-8
        |ja_JP.UTF-8 UTF-8
        |]

  "archlinux/etc.tar" %> \out -> do
    need . fmap ("archlinux/etc" </>) $
      [ "locale.gen"
      , "sudoers"
      , "pacman.d/mirrorlist"
      ]
    runProc $ "tar -c --owner=root --group=nonroot -f" <:> out <:> "-C archlinux/etc ."

pacmanSetup :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
pacmanSetup = do
  let ?proc = proc
  "pacman/pacman.conf" %> \out -> do
    copyFile' (?projectRoot </> "src/pacman.conf") out

  "pacman/db/" `dir` do
    need ["pacman/pacman.conf"]
    runProcess_ . aur . words $ "-Sy"

dockerSetup :: Rules ()
dockerSetup = do
  let ?proc = proc
  phony "docker/login" . runProcess_ $
    docker ["login", takeDirectory . dropTrailingPathSeparator $ cs Util.registry]

musl :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
musl =
  "musl/lib/" `dir` do
    let ?proc = proc
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
  dockerSetup

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
          , shakeChange = ChangeModtimeAndDigest
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