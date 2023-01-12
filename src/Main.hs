module Main (
  main,
) where

import Manifest (yamls)
import Util (Yaml, YamlType (..), s)
import qualified Util

import Development.Shake
import qualified Development.Shake as Shake

import Control.Applicative ((<|>))
import Control.Monad (void)
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
import Optics (modifying, over, preview, view, (%), _head)
import System.Directory (
  createDirectoryIfMissing,
  getCurrentDirectory,
  makeAbsolute,
  removeDirectoryRecursive,
  setCurrentDirectory,
 )
import System.FilePath (takeDirectory, (</>))
import System.Posix (CMode (CMode), setFileCreationMask)
import System.Posix.Files (setFileCreationMask)
import System.Process.Typed (proc, readProcessStdout_, runProcess_)
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

gitClone :: FilePath -> String -> String -> Rules ()
gitClone dst repo tag =
  phony dst $ do
    mkdir dst
    liftIO $ removeDirectoryRecursive dst
    runProc $ "git clone --branch=" <> tag <:> repo <:> dst
    produces =<< getDirectoryFiles "." [dst <//> "*"]

docker = proc "podman"
aur opts = proc "yay" $ words "--noconfirm --noprovides" <> opts
aurInstall opts = aur $ words "-S --config=pacman/pacman.conf --dbpath=pacman/db" <> opts

nonrootImage :: Rules ()
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

pacmanSetup :: (?projectRoot :: FilePath) => Rules ()
pacmanSetup = do
  "pacman/pacman.conf" %> \out -> do
    mkdir "pacman"
    copyFile' (?projectRoot </> "src/pacman.conf") out

  phony "pacman" $ do
    mkdir "pacman/db"
    need ["pacman/pacman.conf"]
    runProcess_ . aur . words $ "-Sy --config=pacman/pacman.conf --dbpath=pacman/db"

musl :: Rules ()
musl = do
  phony "musl" $ do
    mkdir "musl"
    need ["pacman"]
    runProcess_ . aurInstall $ words "--root=musl musl"

  "musl/lib.sfs" %> \out -> do
    need ["musl"]
    runProc $ "mksquashfs ./musl/usr/lib/musl" <:> out <:> "-noappend"

skalibs :: Rules ()
skalibs = do
  let version = "v2.12.0.1"

  gitClone "skalibs/src" "https://github.com/skarnet/skalibs.git" version

  phony "skalibs/configure" $ do
    need ["skalibs/src"]
    runProc "./skalibs/src/configure --disable-shared --prefix=../"

  phony "skalibs/make" $ do
    need ["skalibs/configure"]
    runProc "make -C ./skalibs/src"
    runProc "make -C ./skalibs/src strip"

  phony "skalibs/lib" $ do
    need ["skalibs/make"]
    runProc "make -C ./skalibs/src install-lib"
    runProc "make -C ./skalibs/src install-sysdeps"

  "skalibs/lib.sfs" %> \out -> do
    need ["skalibs/lib"]
    runProc $ "mksquashfs ./skalibs/lib/skalibs" <:> out <:> "-noappend"

s6PortableUtils :: Rules ()
s6PortableUtils = do
  let version = "v2.2.5.0"

  gitClone "s6-portable-utils/src" "https://github.com/skarnet/s6-portable-utils.git" version

  phony "s6-portable-utils/lib/musl" $ do
    need ["musl/lib.sfs"]
    mkdir "./s6-portable-utils/lib/musl"
    runProc "fusermount -u ./s6-portable-utils/lib/musl || true"
    runProc "squashfuse ./musl/lib.sfs ./s6-portable-utils/lib/musl"

  phony "s6-portable-utils/lib/skalibs" $ do
    need ["skalibs/lib.sfs"]
    mkdir "./s6-portable-utils/lib/skalibs"
    runProc "fusermount -u ./s6-portable-utils/lib/skalibs || true"
    runProc "squashfuse ./skalibs/lib.sfs ./s6-portable-utils/lib/skalibs"

  phony "s6-portable-utils/configure" $ do
    need
      [ "s6-portable-utils/src"
      , "s6-portable-utils/lib/musl"
      , "s6-portable-utils/lib/skalibs"
      ]
    runProcess_ $
      proc
        "./s6-portable-utils/src/configure"
        [ "--prefix=../"
        , "--bindir=../bin"
        , "--enable-static-libc"
        , "--with-sysdeps=../lib/skalibs/sysdeps"
        , "--with-libs=../lib/skalibs"
        , "--with-libs=../lib/musl"
        ]

  phony "s6-portable-utils/make" $ do
    need ["s6-portable-utils/configure"]
    runProc "make -C ./s6-portable-utils/src"
    runProc "make -C ./s6-portable-utils/src  strip"

  phony "s6-portable-utils/bin" $ do
    need ["s6-portable-utils/make"]
    runProc "make -C ./s6-portable-utils/src install-bin"

main :: IO ()
main = do
  setFileCreationMask $ CMode 0o022
  projectRoot <- liftIO $ makeAbsolute =<< getCurrentDirectory
  let ?projectRoot = projectRoot
      ?buildDir = projectRoot </> "build"
  mkdir ?buildDir
  liftIO $ setCurrentDirectory ?buildDir
  shakeArgs
    shakeOptions
      { shakeThreads = 0
      , shakeLint = Just LintBasic
      , shakeColor = True
      , shakeProgress = progressSimple
      }
    $ do
      pacmanSetup
      nonrootImage
      musl
      skalibs
      s6PortableUtils
