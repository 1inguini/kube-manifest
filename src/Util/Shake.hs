module Util.Shake (
  (<:>),
  askCache,
  aur,
  aurInstall,
  cache,
  dir,
  dirFile,
  gitClone,
  listDirectoryRecursive,
  mkdir,
  pacman,
  parallel_,
  producedDirectory,
  runProc,
  tar,
) where

import Control.Exception.Safe (displayException)
import qualified Control.Monad.Catch as Exceptions (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Strict (filterM, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteString.Lazy
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (fromString))
import Development.Shake (
  Action,
  FilePattern,
  RuleResult,
  Rules,
  ShakeValue,
  actionCatch,
  addOracleCache,
  askOracle,
  need,
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
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import System.Directory (createDirectoryIfMissing, listDirectory, removeDirectoryRecursive)
import qualified System.Directory as Sys (doesDirectoryExist, doesFileExist)
import System.FilePath (addTrailingPathSeparator, dropExtension, takeDirectory, (</>))
import System.Posix (GroupID, UserID)
import System.Process.Typed (ProcessConfig, proc, readProcessStdout_, runProcess_)

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

pacOpts :: (?shakeDir :: FilePath) => [String]
pacOpts =
  [ "--config=" <> ?shakeDir </> "pacman/pacman.conf"
  , "--dbpath=" <> ?shakeDir </> "pacman/db"
  , "--noconfirm"
  ]
pacman, aur :: (?shakeDir :: FilePath, ?proc :: String -> [String] -> a) => [String] -> a
pacman = ?proc "pacman" . (pacOpts <>)
aur = ?proc "yay" . ((pacOpts <> ["--noprovides"]) <>)
aurInstall :: (?shakeDir :: FilePath, ?proc :: String -> [String] -> a) => [String] -> a
aurInstall = aur . (["-S"] <>)

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

tar :: MonadIO m => (UserID, GroupID) -> FilePath -> m ByteString.Lazy.ByteString
tar (user, group) dir =
  readProcessStdout_ . proc "tar" $
    [ "-c"
    , "--numeric-owner"
    , "--owner=" <> show user
    , "--group=" <> show group
    , "--exclude=" <> dirFile
    , "--to-stdout"
    , -- , "--file=" <> out
      "--directory=" <> dir -- dropExtension out
    , "."
    ]

parallel_ :: [Action a] -> Action ()
parallel_ = void . parallel

newtype Store (key :: Symbol) value = Store ()
  deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult (Store key value) = value

cache ::
  forall (key :: Symbol) value.
  (KnownSymbol key, ShakeValue value) =>
  ((?key :: String) => Action value) ->
  Rules ()
cache act = void $ addOracleCache $ \(Store () :: Store key value) ->
  let ?key = symbolVal (Proxy :: Proxy key) in act

askCache ::
  forall (key :: Symbol) value.
  (KnownSymbol key, ShakeValue value) =>
  Action value
askCache = askOracle $ Store @key @value ()