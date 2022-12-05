module Development.Shake.Util where

import Control.Exception.Safe (catch, handle, throw)
import qualified Control.Monad.Catch as Exceptions (MonadCatch (catch), MonadThrow (throwM))
import Data.Foldable as Foldable
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Development.Shake (
  Action,
  Lint (LintBasic),
  Rules,
  actionCatch,
  addOracle,
  addOracleCache,
  addTarget,
  getShakeOptionsRules,
  liftIO,
  phony,
  phonys,
 )
import qualified Development.Shake as Shake (need, (%>))
import Development.Shake.Command (CmdArgument (CmdArgument), IsCmdArgument (toCmdArgument))
import Development.Shake.Rule (
  BuiltinRun,
  RunChanged (ChangedNothing, ChangedRecomputeDiff, ChangedRecomputeSame),
  RunMode (RunDependenciesChanged, RunDependenciesSame),
  RunResult (RunResult),
  addBuiltinRule,
  addUserRule,
  apply,
  apply1,
  getUserRuleOne,
  noLint,
 )
import Path
import Path.IO (
  createDir,
  createDirIfMissing,
  doesPathExist,
  ensureDir,
  getPermissions,
  removeFile,
  setOwnerWritable,
  setPermissions,
 )
import System.IO.Error (IOError, isDoesNotExistError)
import Prelude hiding (writeFile)

-- import Development.Shake.Plus hiding (CmdOption (Env), addOracle, addOracleCache, phony, (%>))
-- import qualified Development.Shake.Plus as Shake (CmdOption (Env), (%>))

instance Exceptions.MonadThrow Action where
  throwM = liftIO . Exceptions.throwM

instance Exceptions.MonadCatch Action where
  catch = actionCatch

instance Exceptions.MonadThrow Rules where
  throwM = liftIO . Exceptions.throwM

infixl 4 %>
(%>) :: Path b File -> (forall b. Path b File -> Action ()) -> Rules ()
(%>) file act = toFilePath file Shake.%> const (act file)

need :: Foldable t => t (Path a File) -> Action ()
need = Shake.need . fmap toFilePath . Foldable.toList
needIn :: (?workdir :: Path a Dir, Functor t, Foldable t) => t (Path Rel File) -> Action ()
needIn = need . fmap (?workdir </>)

writeFile :: Path a File -> Text -> Rules ()
writeFile path content =
  path %> \path -> do
    ensureDir $ parent path
    ( do
        permission <- getPermissions path
        setPermissions path $ setOwnerWritable True permission
        removeFile path
      )
      `catch` \case
        (e :: IOError) | isDoesNotExistError e -> pure ()
        e -> throw e
    liftIO $ Text.writeFile (toFilePath path) content
writeFileLines :: Path a File -> [Text] -> Rules ()
writeFileLines path = writeFile path . Text.unlines
writeFileIn :: (?workdir :: Path a Dir) => Path Rel File -> Text -> Rules ()
writeFileIn path = writeFile (?workdir </> path)
writeFileLinesIn :: (?workdir :: Path a Dir) => Path Rel File -> [Text] -> Rules ()
writeFileLinesIn path = writeFileLines (?workdir </> path)