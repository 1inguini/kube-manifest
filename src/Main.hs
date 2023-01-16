module Main (
  main,
) where

import Manifest (yamls)
import Util (Yaml, YamlType (..), s)
import qualified Util

import Control.Applicative ((<|>))
import Control.Monad (filterM, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (execState, get)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (AsValue (_Object, _String), key, _Key)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (foldlM, traverse_)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (IsString (fromString))
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Texs
import qualified Data.Yaml as Yaml (decodeAllThrow, encode)
import Development.Shake
import qualified Development.Shake as Shake
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
import System.FilePath (takeDirectory, (</>))
import System.Posix (CMode (CMode), setFileCreationMask)
import System.Posix.Files (setFileCreationMask)
import System.Process.Typed (proc, readProcessStdout_, readProcess_, runProcess_, setWorkingDir)
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
dir :: FilePattern -> ((?dir :: FilePath) => Action ()) -> Rules ()
dir pat act =
  pat </> dirFile %> \out ->
    let ?dir = takeDirectory out
     in do
          act
          ls <-
            filterM (\file -> (file /= dirFile &&) <$> liftIO (Sys.doesFileExist file))
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

nonrootImage :: (?shakeDir :: FilePath) => Rules ()
nonrootImage = do
  phony "nonroot" $ do
    let pause = "./s6-portable-utils/bin/s6-pause"
    need ["nonroot.tar", takeDirectory pause]
    container <-
      fmap (init . cs) . readProcessStdout_ . docker . words $
        "run --detach --volume=" <> pause <> ":/pause --entrypoint=/pause" <:> cs Util.registry </> "scratch"
    traverse_
      (runProcess_ . docker)
      [ words $ "cp nonroot.tar" <:> container <> ":/"
      , ["commit", "--change", "ENTRYPOINT /bin/sh", container, cs Util.registry </> "nonroot"]
      , words $ "stop --time=0" <:> container
      , words $ "rm" <:> container
      ]

  "nonroot.tar" %> \out -> do
    need $ ("nonroot/rootfs/etc/" </>) <$> ["passwd", "group"]
    void $
      parallel
        [ mkdir "nonroot/rootfs/tmp"
        , mkdir "nonroot/rootfs/home/nonroot"
        ]
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

skalibs :: Rules ()
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

s6PortableUtils :: Rules ()
s6PortableUtils = do
  let version = "v2.2.5.0"

  ("s6-portable-utils/bin" </>) <$> ["s6-pause"] &%> \outs -> do
    gitClone "https://github.com/skarnet/s6-portable-utils.git" version "s6-portable-utils/src"

    need ["musl/lib.dir", "skalibs/lib.dir"]
    -- need ["musl/lib.sfs"]
    -- mkdir "./s6-portable-utils/lib/musl"
    -- runProc "squashfuse ./musl/lib.sfs ./s6-portable-utils/lib/musl"
    -- runAfter $ runProc "fusermount -u ./s6-portable-utils/lib/musl"

    -- need ["skalibs/lib.sfs"]
    -- mkdir "./s6-portable-utils/lib/skalibs"
    -- runProc "squashfuse ./skalibs/lib.sfs ./s6-portable-utils/lib/skalibs"
    -- runAfter $ runProc "fusermount -u ./s6-portable-utils/lib/skalibs || true"

    runProcess_ $
      proc
        "./s6-portable-utils/src/configure"
        [ "--prefix=../"
        , "--bindir=../bin"
        , "--enable-static-libc"
        , "--with-sysdeps=../../skalibs/sysdeps"
        , "--with-libs=../../skalibs/lib"
        , "--with-libs=../../musl/lib"
        ]

    runProc "make -C ./s6-portable-utils/src all"
    runProc "make -C ./s6-portable-utils/src strip"

    parallel_
      [ producedDirectory "s6-portable-utils/src"
      , runProc "make -C ./s6-portable-utils/src install-bin"
      ]

rules :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
rules = do
  action $ runAfter $ liftIO $ setCurrentDirectory ?projectRoot
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
        $ \case
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
              shakeShare <- traverse makeAbsolute shakeShare'
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