module Util.Shake (
  aurProgram,
  copyFileContent,
  copyPath,
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
  needExe,
  needPacman,
  nonrootGid,
  nonrootOwn,
  nonrootUid,
  pacmanProgram,
  pacmanSetup,
  parallel_,
  rootGid,
  rootOwn,
  rootUid,
  runProg,
  tar,
  withRoot,
) where

import Control.Exception.Safe (displayException, throwString, try)
import qualified Control.Monad.Catch as Exceptions (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (void)
import qualified Data.ByteString as ByteString
import Development.Shake (
  Action,
  CmdOption (Cwd),
  CmdResult,
  Exit,
  FilePattern,
  Rules,
  StdoutTrim (StdoutTrim),
  actionCatch,
  command,
  doesDirectoryExist,
  getDirectoryContents,
  need,
  parallel,
  phony,
  produces,
  putError,
  withoutTargets,
  writeFileLines,
  (%>),
 )
import GHC.IO.Exception (IOException (IOError))
import System.Directory (
  copyFile,
  copyPermissions,
  createDirectoryIfMissing,
  createDirectoryLink,
  createFileLink,
  getSymbolicLinkTarget,
  listDirectory,
  pathIsSymbolicLink,
  removeDirectoryLink,
  removeDirectoryRecursive,
  removeFile,
 )
import qualified System.Directory as Sys (doesDirectoryExist)
import System.FilePath (
  addTrailingPathSeparator,
  dropExtension,
  dropFileName,
  dropTrailingPathSeparator,
  hasTrailingPathSeparator,
  takeDirectory,
  (<.>),
  (</>),
 )
import System.Posix (
  GroupID,
  UserID,
  fileGroup,
  fileOwner,
  getFileStatus,
  getSymbolicLinkStatus,
  isSymbolicLink,
  setEffectiveUserID,
  setOwnerAndGroup,
 )

instance Exceptions.MonadThrow Action where
  throwM err = do
    putError $ displayException err
    liftIO $ Exceptions.throwM err
instance Exceptions.MonadCatch Action where
  catch = actionCatch
instance Exceptions.MonadThrow Rules where
  throwM = liftIO . Exceptions.throwM

type Owner = (UserID, GroupID)

nonrootOwn :: Owner
nonrootOwn = (nonrootUid, nonrootGid)
nonrootUid :: UserID
nonrootUid = 65532
nonrootGid :: GroupID
nonrootGid = 65532

rootOwn :: Owner
rootOwn = (rootUid, rootGid)
rootUid :: UserID
rootUid = 0
rootGid :: GroupID
rootGid = 0

pacmanProgram, aurProgram :: String
pacmanProgram = "pacman"
aurProgram = "yay"

needExe :: String -> Action FilePath
needExe command = do
  need ["/bin/env"]
  StdoutTrim path <- runProg [] ["/bin/env", "which", command]
  need [path]
  pure path

pacArgs :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => [String]
pacArgs =
  [ "--noconfirm"
  , "--config=" <> ?projectRoot </> "src/build/pacman.conf"
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
    need [?projectRoot </> "src/build/pacman.conf"]
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
  isSymlink <- liftIO $ pathIsSymbolicLink src
  if isDir
    then do
      status <- liftIO $ getFileStatus src
      if isSymlink
        then liftIO $ do
          target <- getSymbolicLinkTarget src
          void $ try @_ @IOError $ removeDirectoryLink dst
          createDirectoryLink target dst
        else do
          mkdir dst
          contents <- if isSymlink then pure [] else getDirectoryContents src
          let par = fmap $ \content -> copyPath (src </> content) (dst </> content)
          parallel_ . par $ contents
      liftIO $ do
        setOwnerAndGroup dst (fileOwner status) (fileGroup status)
        copyPermissions src dst
    else do
      liftIO $ do
        void $ try @_ @IOError $ removeFile dst
        mkdir $ dropFileName dst
      if isSymlink
        then liftIO $ do
          target <- getSymbolicLinkTarget src
          createFileLink target dst
        else do
          need [src]
          liftIO $ copyFile src dst

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
          else do
            isSymlink <- liftIO $ pathIsSymbolicLink fullPath
            pure $ if isSymlink then [] else [content]
  isSymlink <- liftIO $ pathIsSymbolicLink dir
  if isSymlink
    then pure []
    else do
      ls <- getDirectoryContents dir
      concat <$> parallel (par ls)

getDirectoryFilesRecursivePrefixed :: FilePath -> Action [FilePath]
getDirectoryFilesRecursivePrefixed dir =
  fmap (dir </>) . filter (not . hasTrailingPathSeparator) <$> getDirectoryContentsRecursive dir

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
      void $ try @_ @IOError $ liftIO $ removeDirectoryRecursive ?dir
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
  runProg @() [Cwd dst] $ words "git fetch --depth=1 --no-tags" <> [repo, ref]
  runProg @() [Cwd dst] $ words "git reset --hard FETCH_HEAD"

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
