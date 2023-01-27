{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Util.Shake (
  copyFileContent,
  copyPath,
  dir,
  gitClone,
  mkdir,
  needExe,
  needPacman,
  nonrootGid,
  nonrootOwn,
  nonrootUid,
  pacmanSetup,
  parallel_,
  rootOwn,
  runProg,
  tar,
  withRoot,
 )
import Util.Shake.Container (
  ContainerId,
  ImageName (ImageName),
  ImageTag (ImageTag),
  addContainerImageRule,
  dockerCommit,
  dockerCommitSquash,
  dockerCopy,
  dockerExec,
  dockerImport,
  dockerPull,
  dockerPush,
  dockerSetup,
  image,
  latest,
  needDocker,
  withContainer,
 )
import qualified Util.Shake.Container as Image

import Control.Exception.Safe (finally, throwString)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Char as Char
import Data.Foldable (traverse_)
import Development.Shake (
  Action,
  Change (ChangeModtimeAndDigest),
  CmdOption (Cwd),
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
  addTarget,
  getDirectoryContents,
  getShakeOptions,
  need,
  phony,
  progressSimple,
  shakeArgsOptionsWith,
  shakeOptions,
  want,
  withoutTargets,
  writeFile',
  (%>),
  (&%>),
 )
import Safe (headMay)
import System.Directory (
  getCurrentDirectory,
  makeAbsolute,
  setCurrentDirectory,
 )
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitSuccess)
import System.FilePath (
  (</>),
 )
import System.Posix (
  UserID,
  getEnv,
  setEffectiveUserID,
  setFileCreationMask,
  setFileMode,
 )
import System.Process (callProcess)
import Text.Heredoc (here, str)

scratchImage :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
scratchImage = do
  let scratch = ?projectRoot </> "src/scratch.tar"
  Image.registry "scratch" `image` do
    docker <- needDocker
    need [scratch]
    runProg @() [] $ docker ["load", "--input", scratch]
    dockerPush

nonrootImage :: (?shakeDir :: FilePath) => Rules ()
nonrootImage = do
  Image.registry "nonroot" `image` do
    dockerImport "nonroot/rootfs.tar" ["USER nonroot", "WORKDIR /home/nonroot"] ?imageName
    dockerPush

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

archlinuxImage :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
archlinuxImage = do
  let officalImage = ImageName (Image.dockerIo "library/archlinux", ImageTag "base-devel")
      rootExec, nonrootExec :: (?container :: ContainerId) => [String] -> [String] -> Action ()
      rootExec opt = dockerExec (["--user=root"] <> opt)
      nonrootExec opt = dockerExec (["--user=nonroot"] <> opt)

  dockerPull officalImage

  Image.shake "archlinux/files-added" `image` do
    officalImage `withContainer` [] $ do
      parallel_
        [ dockerCopy "archlinux/etc.tar" "/etc/"
        , dockerCopy "pacman/sync.tar" "/var/lib/pacman/sync"
        , dockerCopy "archlinux/aur-helper.tar" "/home/nonroot/aur-helper/"
        ]
      dockerCommit

  Image.shake "archlinux/users-added" `image` do
    ImageName (Image.shake "archlinux/files-added", latest) `withContainer` [] $ do
      rootExec [] ["groupadd", "nonroot", "--gid=" <> show nonrootGid]
      rootExec [] $
        words "useradd nonroot -m -s /usr/bin/nologin"
          <> ["--gid=" <> show nonrootGid, "--uid=" <> show nonrootUid]
      dockerCommit

  Image.shake "archlinux/aur-added" `image` do
    ImageName (Image.shake "archlinux/users-added", latest) `withContainer` [] $ do
      rootExec [] $ words "pacman --noconfirm -S git glibc moreutils rsync"
      parallel_
        [ rootExec [] ["locale-gen"]
        , nonrootExec ["--workdir=/home/nonroot/aur-helper"] ["makepkg", "--noconfirm", "-sir"]
        ]
      let ?instructions = ?instructions <> ["ENV LANG=en_US.UTF-8"]
      dockerCommit

  Image.registry "archlinux" `image` do
    ImageName (Image.shake "archlinux/aur-added", latest) `withContainer` [] $ do
      let ?instruction = ?instructions <> ["USER nonroot"]
      dockerCommitSquash
      dockerPush

  "archlinux/aur-helper/.git"
    `gitClone` ("https://aur.archlinux.org/yay-bin.git", "refs/heads/master")

  "archlinux/aur-helper.tar" %> \out -> do
    need ["archlinux/aur-helper/.git"]
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

  "archlinux/etc/pacman.conf" %> \out ->
    copyFileContent (?projectRoot </> "src/pacman.conf") out

  "archlinux/etc.tar" %> \out -> do
    need . fmap ("archlinux/etc" </>) $
      [ "locale.gen"
      , "sudoers"
      , "pacman.conf"
      , "pacman.d/mirrorlist"
      ]
    tar rootOwn out

openjdk :: (?projectRoot :: FilePath, ?uid :: UserID, ?shakeDir :: FilePath) => Rules ()
openjdk = do
  "openjdk/package/" `dir` do
    pacman <- needPacman
    withRoot . runProg @() [] $ pacman ["-S", "--root=openjdk/package", "jre-openjdk-headless"]

  "openjdk/rootfs" `dir` do
    need ["openjdk/package/"]
    let prefix' = "openjdk/package/usr/lib/jvm/"
    children <- getDirectoryContents prefix'
    usr <- case headMay children of
      Nothing -> throwString $ "there is no directory matching `" <> prefix' <> "*`"
      Just child -> pure $ prefix' </> child
    withRoot $
      parallel_
        [ copyPath "openjdk/package/etc" (?dir </> "etc")
        , copyPath usr (?dir </> "usr")
        ]

  "openjdk/rootfs.tar" %> \out -> do
    need ["openjdk/rootfs/"]
    tar nonrootOwn out

  Image.registry "openjdk" `image` do
    ImageName (Image.registry "nonroot", latest) `withContainer` [] $ do
      dockerCopy "openjdk/rootfs.tar" "/"
      let ?instructions = ?instructions <> [[here|ENTRYPOINT [ "java", "-jar" ]|]]
      dockerCommitSquash
      dockerPush

musl :: (?projectRoot :: FilePath, ?uid :: UserID, ?shakeDir :: FilePath) => Rules ()
musl = do
  "musl/rootfs/" `dir` do
    pacman <- needPacman
    withRoot . runProg @() [] $ pacman ["-S", "--root=musl/rootfs", "musl"]

  "musl/lib/" `dir` do
    need ["musl/rootfs/"]
    copyPath "musl/rootfs/usr/lib/musl/lib" "musl/lib"

skalibs :: (?shakeDir :: FilePath, ?uid :: UserID) => Rules ()
skalibs = do
  let version = "v2.12.0.1"
  "skalibs/src/.git" `gitClone` ("https://github.com/skarnet/skalibs.git", "refs/tags" </> version)

  let cd = Cwd "skalibs/src"
  "skalibs/src/config.mak" %> \out -> do
    need ["skalibs/src/.git"]
    runProg @() [cd] $
      [ "./configure"
      , "--disable-shared"
      , "--libdir=" <> ?shakeDir </> "skalibs/lib"
      , "--sysdepdir=" <> ?shakeDir </> "skalibs/sysdeps"
      ]
  "skalibs/src/sysdeps.cfg/" `dir` need ["skalibs/src/config.mak"]

  let make = runProg @() [cd] . ("make" :) . (: [])
  "skalibs/lib/" `dir` do
    need ["skalibs/src/config.mak"]
    make "strip"
    make "install-lib"

  "skalibs/sysdeps/" `dir` do
    need ["skalibs/src/sysdeps.cfg/"]
    make "install-sysdeps"

s6PortableUtils :: (?shakeDir :: FilePath) => Rules ()
s6PortableUtils = do
  let version = "v2.2.5.0"

  "s6-utils/src/.git"
    `gitClone` ("https://github.com/skarnet/s6-portable-utils.git", "refs/tags" </> version)

  let cd = Cwd "s6-utils/src"
  "s6-utils/src/config.mak" %> \out -> do
    need ["s6-utils/src/.git", "musl/lib/", "skalibs/lib/", "skalibs/sysdeps/"]
    runProg @() [cd] $
      [ "./configure"
      , "--enable-static-libc"
      , "--bindir=" <> ?shakeDir </> "s6-utils/bin"
      , "--with-sysdeps=" <> ?shakeDir </> "skalibs/sysdeps"
      , "--with-libs=" <> ?shakeDir </> "skalibs/lib"
      , "--with-libs=" <> ?shakeDir </> "musl/lib"
      ]

  addTarget "s6-utils/bin/<binary-name>"
  withoutTargets $
    ("s6-utils/bin" </>)
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
        need ["musl/lib/", "skalibs/lib/", "skalibs/sysdeps/", "s6-utils/src/config.mak"]
        let make = runProg [cd] . ("make" :) . (: [])
        make "strip"
        make "install-bin"

busybox :: Rules ()
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

manifests :: (?projectRoot :: FilePath) => Rules ()
manifests = do
  phony "manifest" $ do
    need $ (?projectRoot </>) <$> ["src/Manifest.hs"]
    cabal <- needExe "cabal"
    jobs <- shakeThreads <$> getShakeOptions
    runProg [] [cabal, "run", "-j" <> show jobs, "manifest"]

rules :: (?projectRoot :: FilePath, ?uid :: UserID, ?shakeDir :: FilePath) => Rules ()
rules = do
  addContainerImageRule

  pacmanSetup
  dockerSetup

  openjdk
  musl
  skalibs
  s6PortableUtils
  busybox

  scratchImage
  nonrootImage
  archlinuxImage

  manifests

main :: IO ()
main = do
  exe <- getExecutablePath
  args <- getArgs
  uid <-
    maybe
      ( do
          callProcess "sudo" $ "--preserve-env" : exe : args
          exitSuccess
      )
      (pure . read)
      =<< getEnv "SUDO_UID"
  void $ setFileCreationMask 0o022
  projectRoot <- liftIO $ makeAbsolute =<< getCurrentDirectory
  liftIO $ setEffectiveUserID uid
  let ?uid = uid
      ?projectRoot = projectRoot
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