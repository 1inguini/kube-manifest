module Main where

import Util (
  getCurrentOwn,
  nobodyGid,
  nobodyUid,
  nonrootGid,
  nonrootOwn,
  nonrootUid,
  registry,
  rootOwn,
 )
import Util.Shake (
  aur,
  aurInstall,
  copyDir,
  dir,
  dirFile,
  gitClone,
  gitCloneRule,
  listDirectoryRecursive,
  mkdir,
  parallel_,
  runProg,
  runProg_,
  (<:>),
 )
import qualified Util.Shake.Container as Container (
  ImageName (ImageName),
  ImageRepo (ImageRepo),
  ImageTag (ImageTag),
  addContainerImageRule,
  commit,
  copyDirPrefixed,
  from,
  image,
  latest,
  mount,
  pushEnd,
  runtime,
 )
import qualified Util.Shake.Container as Image

import Control.Exception.Safe (finally)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Char as Char
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.String.Conversions (cs)
import Development.Shake (
  Action,
  Change (ChangeModtimeAndDigest),
  CmdOption (Cwd),
  CmdResult,
  Lint (LintBasic),
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
  copyFile',
  need,
  phony,
  produces,
  progressSimple,
  readFile',
  shakeArgsOptionsWith,
  shakeOptions,
  want,
  writeFile',
  writeFileLines,
  (%>),
  (&%>),
 )
import System.Directory (
  getCurrentDirectory,
  makeAbsolute,
  setCurrentDirectory,
 )
import System.FilePath (
  dropTrailingPathSeparator,
  takeDirectory,
  (<.>),
  (</>),
 )
import System.Posix (
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
--     ( if path `elehere, m` written
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
  let ?owner = nonrootOwn
  Image.registry "nonroot" `Container.image` do
    Container.ImageName (Container.ImageRepo $ cs Util.registry </> "scratch", Container.latest) `Container.from` [] $ Container.mount $ do
      Container.copyDirPrefixed "nonroot/rootfs" "/"
      Container.commit
        [ [here|ENTRYPOINT [ "/bin/sh" ]|]
        , "WORKDIR /home/nonroot"
        , "USER nonroot"
        , [here|LABEL org.opencontainers.image.description="scratch with nonroot user"|]
        ]
      Container.pushEnd

  ("nonroot/rootfs" </> dirFile) %> \out -> do
    let
      rootfs = takeDirectory out
      files = ["etc/passwd", "etc/group"]
      dirs = ["tmp", "home/nonroot"]
    need $ (rootfs </>) <$> files
    parallel_ $ mkdir . (rootfs <>) <$> dirs
    writeFileLines out . List.sort $ files <> dirs

  writeFile' "nonroot/rootfs/etc/passwd" $
    unlines
      [ "root:x:0:0:root:/root:/sbin/nologin"
      , "nobody:x:" <> show nobodyUid <> ":" <> show nobodyGid <> ":Nobody:/nonexistent:/sbin/nologin"
      , "nonroot:x:" <> show nonrootUid <> ":" <> show nonrootGid <> ":nonroot:/home/nonroot:/sbin/nologin"
      ]

  writeFile' "nonroot/rootfs/etc/group" $
    unlines
      [ "root:x:0:"
      , "nobody:x:" <> show nobodyGid <> ":"
      , "nonroot:x:" <> show nonrootGid <> ":"
      ]

archlinuxImage :: (?opts :: [CmdOption], ?shakeDir :: FilePath) => Rules ()
archlinuxImage = do
  Image.registry "archlinux" `Container.image` do
    Container.ImageName (Image.dockerIo "library/archlinux", Container.ImageTag "base-devel") `Container.from` [] $ Container.mount $ do
      let
        podmanExec :: CmdResult r => [String] -> Action r
        podmanExec = Container.runtime . (words "exec -i" <>)
        rootExec, nonrootExec :: (CmdResult r, ?args :: [String]) => [String] -> Action r
        rootExec = podmanExec . (?args <>) . (["--user=root", ?container] <>)
        nonrootExec = podmanExec . (?args <>) . (["--user=nonroot", ?container] <>)
        rootCopy, copy :: FilePath -> FilePath -> Action ()
        rootCopy = let ?owner = rootOwn in Container.copyDirPrefixed
        copy = let ?owner = nonrootOwn in Container.copyDirPrefixed
      let ?args = []
      parallel_
        [ rootCopy "archlinux/etc" "/etc"
        , rootCopy "pacman/db/sync" "/var/lib/pacman/sync"
        , copy "archlinux/aur-helper" "/home/nonroot/aur-helper"
        ]
      parallel_
        [ do
            rootExec @() . words $ "groupadd nonroot" <:> "--gid" <:> show nonrootGid
            rootExec @() . words $
              "useradd nonroot"
                <:> "--gid"
                <:> show nonrootGid
                <:> "--uid"
                <:> show nonrootUid
                <:> "-m -s /usr/bin/nologin"
        , rootExec $ words "pacman --noconfirm -S git glibc moreutils rsync"
        ]
      parallel_
        [ let ?args = ["--workdir=/home/nonroot/aur-helper"] in nonrootExec @() ["makepkg", "--noconfirm", "-sir"]
        , rootExec ["locale-gen"]
        ]
      src <- fmap lines . readFile' $ "archlinux/aur-helper" </> dirFile
      current <- listDirectoryRecursive "archlinux/aur-helper"
      produces $ filter (`notElem` (dirFile : src)) current
      Container.commit
        [ [here|ENTRYPOINT [ "/bin/bash" ]|]
        , [here|CMD [ ]|]
        , "WORKDIR /home/nonroot"
        , "USER nonroot"
        , [here|LABEL org.opencontainers.image.description="archlinux"|]
        ]
      Container.pushEnd

  "archlinux/aur-helper/" `gitCloneRule` ("https://aur.archlinux.org/yay-bin.git", "master")

  ("archlinux/etc" </> dirFile) %> \out -> do
    let ls = ["locale.gen", "sudoers", "pacman.conf", "pacman.d/mirrorlist"]
    need $ (takeDirectory out </>) <$> ls
    writeFileLines out ls

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

  "archlinux/etc/pacman.conf" %> \out ->
    copyFile' "pacman/pacman.conf" out

pacmanSetup :: (?opts :: [CmdOption], ?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
pacmanSetup = do
  "pacman/pacman.conf" %> \out -> do
    copyFile' (?projectRoot </> "src/pacman.conf") out
    liftIO $ setFileMode out 0o644

  "pacman/db/" `dir` do
    need ["pacman/pacman.conf"]
    aur . words $ "-Sy"

  "pacman/db/sync" </> dirFile %> \out -> do
    -- liftIO $ setUserID rootUid
    need ["pacman/db" </> dirFile]
    ls <- listDirectoryRecursive $ takeDirectory out
    -- liftIO $ flip withAsync wait $ do
    --   setEffectiveUserID rootUid
    --   putStrLn . ("getRealUserID" <:>) . show =<< getRealUserID
    --   putStrLn . ("getEffectiveUserID" <:>) . show =<< getEffectiveUserID
    writeFileLines out ls

podmanSetup :: (?opts :: [CmdOption]) => Rules ()
podmanSetup = do
  phony "podman/login" $
    Container.runtime ["login", takeDirectory . dropTrailingPathSeparator $ cs Util.registry]

musl :: (?opts :: [CmdOption], ?shakeDir :: FilePath) => Rules ()
musl = do
  "musl/rootfs" `dir` do
    need ["pacman/db" </> dirFile]
    aurInstall @() ["--root=musl/rootfs", "musl"]

  "musl/rootfs/usr/lib/musl/lib" </> dirFile %> \out -> do
    need ["musl/rootfs" </> dirFile]
    writeFileLines out =<< listDirectoryRecursive (takeDirectory out)

  "musl/lib/" `dir` do
    owner <- liftIO getCurrentOwn
    let ?owner = owner
    copyDir "musl/rootfs/usr/lib/musl/lib" "musl/lib"

skalibs :: (?opts :: [CmdOption], ?shakeDir :: FilePath) => Rules ()
skalibs = do
  let version = "v2.12.0.1"
  let make = runProg_ . ("make" :) . (: [])
  let ?opts = Cwd "skalibs/src" : ?opts

  "skalibs/src/" `gitCloneRule` ("https://github.com/skarnet/skalibs.git", version)

  "skalibs/src/config.mak" %> \out -> do
    need ["skalibs/src" </> dirFile <.> "git"]
    runProg_
      [ "./configure"
      , "--disable-shared"
      , "--libdir=" <> ?shakeDir </> "skalibs/lib"
      , "--sysdepdir=" <> ?shakeDir </> "skalibs/sysdeps"
      ]
    produces
      [ "skalibs/src/src/include/skalibs/config.h"
      , "skalibs/src/sysdeps.cfg/socket.lib"
      , "skalibs/src/sysdeps.cfg/spawn.lib"
      , "skalibs/src/sysdeps.cfg/sysclock.lib"
      , "skalibs/src/sysdeps.cfg/sysdeps"
      , "skalibs/src/sysdeps.cfg/target"
      , "skalibs/src/sysdeps.cfg/timer.lib"
      , "skalibs/src/sysdeps.cfg/util.lib"
      ]

  "skalibs/lib/libskarnet.a" %> \out -> do
    need ["skalibs/src/config.mak"]
    make "strip"
    make "install-libs"

  "skalibs/sysdeps/sysdeps" %> \out -> do
    need ["skalibs/src/config.mak"]
    make "install-sysdeps"
    produces
      [ "skalibs/sysdeps/socket.lib"
      , "skalibs/sysdeps/spawn.lib"
      , "skalibs/sysdeps/sysclock.lib"
      , "skalibs/sysdeps/target"
      , "skalibs/sysdeps/timer.lib"
      , "skalibs/sysdeps/util.lib"
      ]

s6PortableUtils :: (?opts :: [CmdOption], ?shakeDir :: FilePath) => Rules ()
s6PortableUtils = do
  let version = "v2.2.5.0"
  let s6 = "s6-portable-utils"
  let make = runProg . ("make" :) . (: [])
  let ?opts = Cwd (s6 </> "src") : ?opts

  (s6 </> "src/") `gitCloneRule` ("https://github.com/skarnet/s6-portable-utils.git", version)

  s6 </> "src/config.mak" %> \out -> do
    need [s6 </> "src" </> dirFile <.> "git"]
    runProg_
      [ "./configure"
      , "--bindir=" <> ?shakeDir </> s6 </> "bin"
      , "--enable-static-libc"
      , "--with-libs=" <> ?shakeDir </> "musl/lib"
      , "--with-libs=" <> ?shakeDir </> "skalibs/lib"
      , "--with-sysdeps=" <> ?shakeDir </> "skalibs/sysdeps"
      ]
    produces [s6 </> "src/src/include/s6-portable-utils/config.h"]

  (s6 </>) . ("bin" </>)
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
    &%> \outs -> do
      need
        [ s6 </> "src/config.mak"
        , "musl/lib" </> dirFile
        , "skalibs/lib/libskarnet.a"
        , "skalibs/sysdeps/sysdeps"
        ]
      make "strip"
      make "install-bin"

busybox :: (?opts :: [CmdOption]) => Rules ()
busybox = download "busybox" "busybox" *> traverse_ singleApplet applets
 where
  version = "1.35.0"
  applets = ["cp"]
  download applet src =
    ("busybox/bin" </> applet) %> \out -> do
      runProg_
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
  Container.addContainerImageRule

  pacmanSetup
  podmanSetup

  musl
  skalibs
  s6PortableUtils
  busybox

  nonrootImage
  archlinuxImage

main :: IO ()
main = do
  void $ setFileCreationMask 0o022
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