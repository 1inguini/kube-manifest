module Util.Shake (
  (<:>),
  gitClone,
  aur,
  aurInstall,
  docker,
  listDirectoryRecursive,
  dirFile,
  dir,
  parallel_,
  mkdir,
  runProc,
  producedDirectory,
) where

import Control.Exception.Safe (displayException)
import qualified Control.Monad.Catch as Exceptions (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (filterM, void)
import Data.String (IsString (fromString))
import Development.Shake (
  Action,
  FilePattern,
  Rules,
  actionCatch,
  need,
  parallel,
  phony,
  produces,
  putError,
  withoutTargets,
  writeFile',
  (%>),
 )
import System.Directory (
  createDirectoryIfMissing,
  listDirectory,
  removeDirectoryRecursive,
 )
import qualified System.Directory as Sys (doesDirectoryExist, doesFileExist)
import System.FilePath (addTrailingPathSeparator, takeDirectory, (</>))
import System.Process.Typed (
  ProcessConfig,
  proc,
  runProcess_,
 )

instance Exceptions.MonadThrow Action where
  throwM err = do
    putError $ displayException err
    liftIO $ Exceptions.throwM err
instance Exceptions.MonadCatch Action where
  catch = actionCatch
instance Exceptions.MonadThrow Rules where
  throwM = liftIO . Exceptions.throwM

runProc :: MonadIO m => String -> m ()
runProc = runProcess_ . fromString

(<:>) :: String -> String -> String
x <:> y = x <> " " <> y

gitClone :: String -> String -> FilePath -> Action ()
gitClone repo tag dst = do
  mkdir dst
  liftIO $ removeDirectoryRecursive dst
  runProc $ "git clone --branch=" <> tag <:> repo <:> dst

docker :: [String] -> ProcessConfig () () ()
docker = proc "podman"
aur :: (?shakeDir :: FilePath) => [String] -> ProcessConfig () () ()
aur opts =
  proc "yay" $
    opts
      <> [ "--config=" <> ?shakeDir </> "pacman/pacman.conf"
         , "--dbpath=" <> ?shakeDir </> "pacman/db"
         , "--noconfirm"
         , "--noprovides"
         ]
aurInstall :: (?shakeDir :: FilePath) => [String] -> ProcessConfig () () ()
aurInstall opts = aur $ ["-S"] <> opts

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
dir :: (?shakeDir :: FilePath) => FilePattern -> ((?dir :: FilePath) => Action ()) -> Rules ()
dir pat act = do
  phony (addTrailingPathSeparator pat) $ need [pat </> dirFile]
  withoutTargets $
    pat </> dirFile %> \out ->
      let ?dir = ?shakeDir </> takeDirectory out
       in do
            act
            ls <-
              filterM (\file -> (file /= dirFile &&) <$> liftIO (Sys.doesFileExist $ ?dir </> file))
                =<< listDirectoryRecursive ?dir
            produces $ (?dir </>) <$> ls
            writeFile' out $ unlines ls

parallel_ :: [Action a] -> Action ()
parallel_ = void . parallel
