module Util.Shake (
  (<:>),
  aurProgram,
  copyDir,
  dir,
  dirTarget,
  getDirectoryContentsRecursive,
  gitClone,
  listDirectoryRecursive,
  mkdir,
  needAur,
  needPacman,
  needPermission,
  needSudo,
  pacmanProgram,
  pacmanSetup,
  parallel_,
  runProg,
  sudoProgram,
  sudoSetup,
  tar,
) where

import Util (Owner, getCurrentOwner, rootOwn)

import Control.Exception.Safe (displayException, throwString, try)
import qualified Control.Monad.Catch as Exceptions (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (void)
import qualified Data.ByteString as ByteString
import Data.Functor (($>))
import Data.String.Conversions (cs)
import Development.Shake (
  Action,
  CmdOption (Cwd, EchoStdout, StdinBS),
  CmdResult,
  Exit (Exit),
  FilePattern,
  Rules,
  StdoutTrim (StdoutTrim),
  actionCatch,
  command,
  copyFile',
  doesDirectoryExist,
  getDirectoryContents,
  need,
  parallel,
  phony,
  produces,
  putError,
  putInfo,
  putWarn,
  readFile',
  withoutTargets,
  writeFileLines,
  (%>),
 )
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import System.Directory (copyFile, createDirectoryIfMissing, listDirectory, removeDirectoryRecursive, removeFile)
import qualified System.Directory as Sys (doesDirectoryExist)
import System.FilePath (addTrailingPathSeparator, dropExtension, dropFileName, hasTrailingPathSeparator, takeDirectory, takeFileName, (<.>), (</>))
import System.Posix (GroupID, UserID, setSymbolicLinkOwnerAndGroup)

instance Exceptions.MonadThrow Action where
  throwM err = do
    putError $ displayException err
    liftIO $ Exceptions.throwM err
instance Exceptions.MonadCatch Action where
  catch = actionCatch
instance Exceptions.MonadThrow Rules where
  throwM = liftIO . Exceptions.throwM

(<:>) :: String -> String -> String
x <:> y = x <> " " <> y

sudoProgram, pacmanProgram, aurProgram :: String
sudoProgram = "sudo"
pacmanProgram = "pacman"
aurProgram = "yay"

needPermission :: Action ()
needPermission = need ["auth"]
needSudo :: Action ([String] -> [String])
needSudo = needPermission $> (sudoProgram :) . (words "--non-interactive --" <>)
sudoSetup :: Rules ()
sudoSetup = do
  phony "auth" $ do
    Exit noNeedPassword <-
      runProg [] $
        sudoProgram : words "--non-interactive --validate"
    case noNeedPassword of
      ExitFailure _ -> do
        putWarn $ "input password for" <:> sudoProgram
        input <- cs <$> liftIO ByteString.getLine
        runProg [EchoStdout False, StdinBS input] $ sudoProgram : words "--validate --stdin"
      ExitSuccess -> pure ()

pacArgs :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => [String]
pacArgs =
  [ "--noconfirm"
  , "--config=" <> ?projectRoot </> "src/pacman.conf"
  , "--dbpath=" <> ?shakeDir </> "pacman"
  ]
needPacman :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Action ([String] -> [String])
needPacman = do
  need ["pacman/sync/.tar"]
  sudo <- needSudo
  pure $ sudo . (pacmanProgram :) . (pacArgs <>)
needAur :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Action ([String] -> [String])
needAur = needPacman $> (aurProgram :) . (["--noprovides"] <>)
pacmanSetup :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
pacmanSetup = do
  "pacman/sync/.tar" %> \out -> do
    need [?projectRoot </> "src/pacman.conf"]
    sudo <- needSudo
    runProg @() [] . sudo $ pacmanProgram : pacArgs <> ["-Sy"]
    dbfiles <- getDirectoryContentsRecursivePrefixed "pacman/sync"
    produces $ "pacman/local/ALPM_DB_VERSION" : dbfiles
    tar rootOwn out

gitClone :: String -> String -> FilePath -> Action ()
gitClone repo tag dst = do
  isRepo <- doesDirectoryExist $ dst </> ".git"
  if isRepo
    then runProg [Cwd dst] $ words "git fetch --depth=1 origin" <> [tag]
    else runProg [] $ words "git clone --depth=1 --single-branch" <> ["--branch=" <> tag, repo, dst]

listGitFiles :: FilePath -> Action [FilePath]
listGitFiles repoDir = do
  StdoutTrim repoFiles <- runProg [Cwd repoDir] $ words "git ls-tree -r --name-only HEAD"
  gitFiles <- fmap (".git" </>) <$> listDirectoryRecursive (repoDir </> ".git")
  pure $ gitFiles <> lines repoFiles

defaultOpts :: [CmdOption]
defaultOpts = []
runProg :: (CmdResult r) => [CmdOption] -> [String] -> Action r
runProg opts (prog : args) = command (defaultOpts <> opts) prog args
runProg _ [] = throwString "runProg: empty"

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

getDirectoryContentsRecursive :: FilePath -> Action [FilePath]
getDirectoryContentsRecursive dir = do
  let par :: [FilePath] -> [Action [FilePath]]
      par = fmap $ \content -> do
        let fullPath = dir </> content
        isDir <- doesDirectoryExist fullPath
        if isDir
          then do
            ls <- getDirectoryContentsRecursive fullPath
            pure . (addTrailingPathSeparator content :) $ (content </>) <$> ls
          else pure [content]
  ls <- getDirectoryContents dir
  concat <$> parallel (par ls)
getDirectoryContentsRecursivePrefixed :: FilePath -> Action [FilePath]
getDirectoryContentsRecursivePrefixed dir = fmap (dir </>) <$> getDirectoryContentsRecursive dir

copyDir :: (?owner :: Owner) => FilePath -> FilePath -> Action ()
copyDir srcdir dstdir = do
  let par = fmap $ \path -> do
        let
          srcfile = srcdir </> path
          dstfile = dstdir </> path
        let
          dstdir = dropFileName dstfile
        liftIO $ do
          mkdir dstdir
          void . try @IO @IOError $ removeFile dstfile -- symlink safety
          copyFile srcfile dstfile
          uncurry (setSymbolicLinkOwnerAndGroup dstfile) ?owner
  paths <- getDirectoryContentsRecursive srcdir
  parallel_ . par $ paths

dirExtention :: String
dirExtention = ".ls"

dirTarget :: String -> String
dirTarget = (<.> dirExtention)

infix 1 `dir`
dir :: FilePattern -> ((?dir :: FilePath) => Action ()) -> Rules ()
dir pat act = do
  let target = dirTarget pat
  phony (addTrailingPathSeparator pat) $ need [target]
  withoutTargets $
    target %> \out -> do
      let ?dir = dropExtension out
      mkdir ?dir
      act
      ls <- getDirectoryContentsRecursive ?dir
      produces $ (?dir </>) <$> filter (not . hasTrailingPathSeparator) ls
      writeFileLines out ls

tar :: (UserID, GroupID) -> FilePath -> Action ()
tar (user, group) out =
  runProg
    []
    [ "tar"
    , "-c"
    , "--numeric-owner"
    , "--owner=" <> show user
    , "--group=" <> show group
    , "--exclude=" <> takeFileName out
    , "--file=" <> out
    , "--directory=" <> dropFileName out
    , "."
    ]

parallel_ :: [Action a] -> Action ()
parallel_ = void . parallel
