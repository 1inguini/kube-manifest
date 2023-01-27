module Util.Shake (
  (<:>),
  aurProgram,
  copyDir,
  dir,
  dirTarget,
  getDirectoryContentsRecursive,
  getDirectoryFilesRecursivePrefixed,
  getGitFilesPrefixed,
  gitClone,
  gitCloneAction,
  listDirectoryRecursive,
  mkdir,
  needAur,
  needPacman,
  needPermission,
  pacmanProgram,
  pacmanSetup,
  parallel_,
  runProg,
  sudoProgram,
  tar,
  needExe,
  copyFileContent,
  copyPath,
  withRoot,
) where

import Util (rootOwn, rootUid)

import Control.Exception.Safe (catch, displayException, throw, throwString, try)
import Control.Monad (unless, when)
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
  doesDirectoryExist,
  doesFileExist,
  getDirectoryContents,
  need,
  parallel,
  phony,
  produces,
  putError,
  putWarn,
  withoutTargets,
  writeFileLines,
  (%>),
 )
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import System.Directory (copyFile, createDirectoryIfMissing, doesPathExist, listDirectory, removeFile)
import qualified System.Directory as Sys (doesDirectoryExist)
import System.FilePath (addTrailingPathSeparator, dropExtension, dropFileName, dropTrailingPathSeparator, hasTrailingPathSeparator, takeDirectory, (<.>), (</>))
import System.IO.Error (isPermissionError)
import System.Posix (GroupID, UserID, setEffectiveUserID)

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

needExe :: String -> Action FilePath
needExe command = do
  need ["/bin/env"]
  StdoutTrim path <- runProg [] ["/bin/env", "which", command]
  need [path]
  pure path

needPermission :: Action ()
needPermission = need ["auth"]

-- needSudo :: Action ([String] -> [String])
-- needSudo = needPermission $> (sudoProgram :) . (words "--non-interactive --" <>)
-- sudoSetup :: Rules ()
-- sudoSetup = do
--   phony "auth" $ do
--     sudo <- needExe sudoProgram
--     Exit noNeedPassword <-
--       runProg [] $
--         sudo : words "--non-interactive --validate"
--     case noNeedPassword of
--       ExitFailure _ -> do
--         putWarn $ "input password for" <:> sudoProgram
--         input <- cs <$> liftIO ByteString.getLine
--         runProg [EchoStdout False, StdinBS input] $ sudo : words "--validate --stdin"
--       ExitSuccess -> pure ()

pacArgs :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => [String]
pacArgs =
  [ "--noconfirm"
  , "--config=" <> ?projectRoot </> "src/pacman.conf"
  , "--dbpath=" <> ?shakeDir </> "pacman"
  ]
needPacman :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Action ([String] -> [String])
needPacman = do
  pacman <- needExe pacmanProgram
  need ["pacman/sync.tar"]
  -- sudo <- needSudo
  -- pure $ sudo . (pacman :) . (pacArgs <>)
  pure $ (pacman :) . (pacArgs <>)
needAur :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Action ([String] -> [String])
needAur = do
  _ <- needPacman
  aur <- needExe aurProgram
  pure $ (aur :) . (["--noprovides"] <>)
pacmanSetup :: (?projectRoot :: FilePath, ?shakeDir :: FilePath, ?uid :: UserID) => Rules ()
pacmanSetup = do
  "pacman/sync.tar" %> \out -> do
    need [?projectRoot </> "src/pacman.conf"]
    -- sudo <- needSudo
    pacman <- needExe pacmanProgram
    dbfiles <- withRoot $ do
      runProg @() [] $ pacman : pacArgs <> ["-Sy"]
      getDirectoryFilesRecursivePrefixed "pacman/sync"
    produces $ "pacman/local/ALPM_DB_VERSION" : dbfiles
    tar rootOwn out

getGitFilesPrefixed :: FilePath -> Action [FilePath]
getGitFilesPrefixed repoDir = do
  StdoutTrim repoFiles <- runProg [Cwd repoDir] $ words "git ls-tree -r --name-only HEAD"
  gitFiles <- getDirectoryFilesRecursivePrefixed (repoDir </> ".git")
  pure $ gitFiles <> (fmap (repoDir </>) . lines) repoFiles

defaultOpts :: [CmdOption]
defaultOpts = []
runProg :: (CmdResult r) => [CmdOption] -> [String] -> Action r
runProg opts (prog : args) = command (defaultOpts <> opts) prog args
runProg _ [] = throwString "runProg: empty"

mkdir :: MonadIO m => FilePath -> m ()
mkdir = liftIO . createDirectoryIfMissing True

-- `copyFile'` keeps permissions, `copyFileContents` will give the new file 644 permission
copyFileContent :: FilePath -> FilePath -> Action ()
copyFileContent src dst = do
  need [src]
  liftIO $ ByteString.readFile src >>= ByteString.writeFile dst

copyPath :: FilePath -> FilePath -> Action ()
copyPath src dst = do
  isDir <- doesDirectoryExist src
  if isDir
    then mkdir dst
    else do
      need [src]
      mkdir $ dropFileName dst
      liftIO $ do
        removeFile dst
        copyFile src dst

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

getDirectoryFilesRecursivePrefixed :: FilePath -> Action [FilePath]
getDirectoryFilesRecursivePrefixed dir =
  fmap (dir </>) . filter (not . hasTrailingPathSeparator) <$> getDirectoryContentsRecursive dir

copyDir :: FilePath -> FilePath -> Action ()
copyDir srcdir dstdir = do
  let par = fmap $ \path -> do
        let srcfile = srcdir </> path
            dstfile = dstdir </> path
        let dstdir = dropFileName dstfile
        liftIO $ do
          mkdir dstdir
          void . try @IO @IOError $ removeFile dstfile -- symlink safety
          copyFile srcfile dstfile
  -- uncurry (setSymbolicLinkOwnerAndGroup dstfile) ?owner
  paths <- getDirectoryContentsRecursive srcdir
  parallel_ . par $ paths

dirExtention :: String
dirExtention = ".ls"
dirTarget :: String -> String
dirTarget = (<.> dirExtention) . dropTrailingPathSeparator

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
      produces =<< getDirectoryFilesRecursivePrefixed ?dir
      writeFileLines out =<< getDirectoryContentsRecursive ?dir

withRoot :: (?uid :: UserID) => Action a -> Action a
withRoot act = do
  liftIO $ setEffectiveUserID rootUid
  result <- act
  liftIO $ setEffectiveUserID ?uid
  pure result

gitCloneAction :: String -> String -> FilePath -> Action ()
gitCloneAction repo ref dst = do
  void $ runProg @Exit [Cwd dst] $ words "git init"
  let gitDir = dst </> ".git"
      git = runProg @() [] . (["git", "--git-dir=" <> gitDir] <>)
  git $ words "fetch --depth=1 --no-tags" <> [repo, ref]
  git $ words "reset --hard FETCH_HEAD"

gitClone :: FilePath -> (String, String) -> Rules ()
gitClone gitDir (repo, ref) = do
  let target = gitDir </> "HEAD"
  phony gitDir $ need [target]
  target %> \out -> do
    let dir = takeDirectory gitDir
    gitCloneAction repo ref dir
    produces =<< getGitFilesPrefixed dir

tar :: (UserID, GroupID) -> FilePath -> Action ()
tar (user, group) out =
  runProg
    []
    [ "tar"
    , "-c"
    , "--numeric-owner"
    , "--owner=" <> show user
    , "--group=" <> show group
    , -- , "--exclude=" <> takeFileName out
      "--exclude=.git"
    , "--file=" <> out
    , "--directory=" <> dropExtension out
    , "."
    ]

parallel_ :: [Action a] -> Action ()
parallel_ = void . parallel
