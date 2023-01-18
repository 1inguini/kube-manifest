module Util.Shake (
  (<:>),
  aur,
  aurInstall,
  dir,
  dirFile,
  gitClone,
  listDirectoryRecursive,
  mkdir,
  pacman,
  parallel_,
  producedDirectory,
  runProg,
  runProg_,
  tar,
  copyDir,
  gitCloneRule,
) where

import Control.Exception.Safe (
  displayException,
  throwString,
  try,
 )
import qualified Control.Monad.Catch as Exceptions (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (filterM, void)
import Data.Foldable (traverse_)
import Development.Shake (
  Action,
  CmdOption,
  CmdResult,
  FilePattern,
  Rules,
  StdoutTrim (StdoutTrim),
  actionCatch,
  command,
  need,
  parallel,
  phony,
  produces,
  putError,
  readFile',
  writeFile',
  writeFileLines,
  (%>),
 )
import System.Directory (
  copyFile,
  createDirectoryIfMissing,
  listDirectory,
  removeDirectoryRecursive,
  removeFile,
 )
import qualified System.Directory as Sys (doesDirectoryExist, doesFileExist)
import System.FilePath (addTrailingPathSeparator, dropExtension, takeDirectory, (<.>), (</>))
import System.Posix (GroupID, UserID, setSymbolicLinkOwnerAndGroup)

import qualified Data.List as List
import Util (Owner)

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

gitClone :: String -> String -> FilePath -> Action ()
gitClone repo tag dst = do
  mkdir dst
  liftIO $ removeDirectoryRecursive dst
  let ?opts = []
  runProg ["git", "clone", "--branch=" <> tag, repo, dst]

gitCloneRule :: (?opts :: [CmdOption]) => FilePath -> (String, String) -> Rules ()
gitCloneRule dst (repo, tag) = do
  (dst </> dirFile <.> "git") %> \out -> do
    gitClone repo tag dst
    StdoutTrim ls <- runProg $ words "git ls-tree -r --name-only" <> [tag]
    git <- fmap (".git" </>) <$> listDirectoryRecursive (dst </> ".git")
    writeFileLines out $ git <> lines ls

runProg :: (CmdResult r, ?opts :: [CmdOption]) => [String] -> Action r
runProg (prog : args) = command ?opts prog args
runProg [] = throwString "runProg: empty"

runProg_ :: (?opts :: [CmdOption]) => [String] -> Action ()
runProg_ = runProg

pacConf :: (?shakeDir :: FilePath) => [String]
pacConf =
  [ "--config=" <> ?shakeDir </> "pacman/pacman.conf"
  , "--dbpath=" <> ?shakeDir </> "pacman/db"
  ]
pacman, aur :: (CmdResult r, ?shakeDir :: FilePath, ?opts :: [CmdOption]) => [String] -> Action r
pacman = runProg . (["pacman", "--noconfirm"] <>) . (pacConf <>)
aur = runProg . (["yay", "--noprovides", "--noconfirm"] <>) . (pacConf <>)
aurInstall :: (CmdResult r, ?shakeDir :: FilePath, ?opts :: [CmdOption]) => [String] -> Action r
aurInstall = aur . (["-S"] <>)

mkdir :: (MonadIO m) => FilePath -> m ()
mkdir path = liftIO $ do
  createDirectoryIfMissing True path

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
  pat </> dirFile %> \out ->
    let ?dir = ?shakeDir </> takeDirectory out
     in do
          act
          ls <-
            filterM (\file -> (file /= dirFile &&) <$> liftIO (Sys.doesFileExist $ ?dir </> file))
              =<< listDirectoryRecursive ?dir
          produces $ (?dir </>) <$> ls
          writeFile' out $ unlines ls

copyDir :: (?owner :: Owner) => FilePath -> FilePath -> Action ()
copyDir srcdir dstdir = do
  paths <- readFile' $ srcdir </> dirFile
  traverse_
    ( \path -> liftIO $ do
        let
          srcfile = srcdir </> path
          dstfile = dstdir </> path
          dstdir' = takeDirectory dstfile
        mkdir dstdir'
        uncurry (setSymbolicLinkOwnerAndGroup dstdir') ?owner
        void . try @IO @IOError $ removeFile dstfile -- symlink safety
        copyFile srcfile dstfile
        uncurry (setSymbolicLinkOwnerAndGroup dstfile) ?owner
    )
    $ lines paths

tar :: (?opts :: [CmdOption]) => (UserID, GroupID) -> FilePath -> Action ()
tar (user, group) out =
  runProg
    [ "tar"
    , "-c"
    , "--numeric-owner"
    , "--owner=" <> show user
    , "--group=" <> show group
    , "--exclude=" <> dirFile
    , "--file=" <> out
    , "--directory=" <> dropExtension out
    , "."
    ]

parallel_ :: [Action a] -> Action ()
parallel_ = void . parallel
