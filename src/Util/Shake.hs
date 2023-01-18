module Util.Shake (
  (<:>),
  dir,
  dirFile,
  gitClone,
  listDirectoryRecursive,
  mkdir,
  parallel_,
  producedDirectory,
  runProg,
  tar,
) where

import Control.Exception.Safe (displayException, throwString)
import qualified Control.Monad.Catch as Exceptions (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (filterM, void)
import Data.ByteString (ByteString)
import Data.String (IsString (fromString))
import Development.Shake (
  Action,
  CmdOption (Cwd),
  CmdResult,
  FilePattern,
  RuleResult,
  Rules,
  StdoutTrim (StdoutTrim),
  actionCatch,
  command,
  need,
  newCache,
  parallel,
  phony,
  produces,
  putError,
  withoutTargets,
  writeFile',
  (%>),
 )
import Development.Shake.Classes (Binary, Hashable, NFData, Typeable)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, listDirectory, removeDirectoryRecursive)
import qualified System.Directory as Sys (doesDirectoryExist, doesFileExist)
import System.FilePath (addTrailingPathSeparator, dropExtension, dropFileName, takeDirectory, takeFileName, (</>))
import System.Posix (GroupID, UserID)

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
  runProg [] ["git", "clone", "--branch=" <> tag, repo, dst]

listGitFiles :: FilePath -> Action [FilePath]
listGitFiles repoDir = do
  StdoutTrim repoFiles <- runProg [Cwd repoDir] $ words "git ls-tree -r --name-only HEAD"
  gitFiles <- fmap (".git" </>) <$> listDirectoryRecursive (repoDir </> ".git")
  pure $ gitFiles <> lines repoFiles

runProg :: (CmdResult r) => [CmdOption] -> [String] -> Action r
runProg opts (prog : args) = command opts prog args
runProg opts [] = throwString "runProg: empty"

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
