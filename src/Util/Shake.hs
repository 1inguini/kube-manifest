module Util.Shake (
  (<:>),
  dir,
  dirFile,
  getDirectoryContentsRecursive,
  gitClone,
  listDirectoryRecursive,
  mkdir,
  parallel_,
  runProg,
  tar,
  producesDirectory,
  copyDir,
  needPermission,
  sudoProgram,
  pacmanProgram,
  aurProgram,
  sudoSetup,
  needSudo,
  needPacman,
  needAur,
  pacmanSetup,
) where

import Util (Owner, rootOwn)

import Control.Exception.Safe (displayException, throwString, try)
import qualified Control.Monad.Catch as Exceptions (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (filterM, void)
import qualified Data.ByteString as ByteString
import Data.Functor (($>))
import Data.String.Conversions (cs)
import Development.Shake (
  Action,
  CmdOption (Cwd, StdinBS),
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
  putWarn,
  readFile',
  withoutTargets,
  writeFile',
  (%>),
 )
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import System.Directory (createDirectoryIfMissing, listDirectory, removeDirectoryRecursive, removeFile)
import qualified System.Directory as Sys (doesDirectoryExist, doesFileExist)
import System.FilePath (addTrailingPathSeparator, dropFileName, takeDirectory, takeFileName, (</>))
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
needSudo = needPermission $> (sudoProgram :) . (words "--non-interactive" <>)
sudoSetup :: Rules ()
sudoSetup = do
  phony "auth" $ do
    Exit noNeedPassword <- runProg [] $ sudoProgram : words "--non-interactive --validate"
    case noNeedPassword of
      ExitFailure _ -> do
        putWarn $ "input password for" <:> sudoProgram
        input <- cs <$> liftIO ByteString.getLine
        runProg [StdinBS input] $ sudoProgram : words "--validate --stdin"
      ExitSuccess -> pure ()

pacArgs :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => [String]
pacArgs =
  [ "--noconfirm"
  , "--config=" <> ?projectRoot </> "src/pacman.conf"
  , "--dbpath=" <> ?shakeDir </> "pacman"
  ]
needPacman :: Action ([String] -> [String])
needPacman = do
  need ["pacman/sync/.tar"]
  sudo <- needSudo
  pure $ sudo . (pacmanProgram :)
needAur :: Action ([String] -> [String])
needAur = needPacman $> (aurProgram :) . (["--noprovides"] <>)
pacmanSetup :: (?projectRoot :: FilePath, ?shakeDir :: FilePath) => Rules ()
pacmanSetup = do
  "pacman/sync/.tar" %> \out -> do
    need [?projectRoot </> "src/pacman.conf"]
    sudo <- needSudo
    runProg @() [] . sudo $ pacmanProgram : pacArgs <> ["-Sy"]
    produces ["pacman/local/ALPM_DB_VERSION"]
    tar rootOwn out

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
      par = fmap $ \path -> do
        let deepPath = dir </> path
        isDir <- doesDirectoryExist deepPath
        if isDir
          then do
            ls <- getDirectoryContentsRecursive deepPath
            pure $ (path </>) <$> ls
          else pure []
  ls <- filter (/= dirFile) <$> getDirectoryContents dir
  concat <$> parallel (par ls)

producesDirectory :: FilePath -> Action [FilePath]
producesDirectory dir = do
  ls <- getDirectoryContentsRecursive dir
  produces $ (dir </>) <$> ls
  pure ls

copyDir :: (?owner :: Owner) => FilePath -> FilePath -> Action ()
copyDir srcdir dstdir = do
  let par = fmap $ \path -> do
        let
          srcfile = srcdir </> path
          dstfile = dstdir </> path
        let
          dstdir = dropFileName dstfile
        mkdir dstdir
        liftIO $ uncurry (setSymbolicLinkOwnerAndGroup dstdir) ?owner
        void . try @Action @IOError $ liftIO $ removeFile dstfile -- symlink safety
        copyFile' srcfile dstfile
        liftIO $ uncurry (setSymbolicLinkOwnerAndGroup dstfile) ?owner
  paths <- readFile' $ srcdir </> dirFile
  parallel_ . par $ lines paths

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
