module Development.Shake.Util where

import Control.Exception.Safe (Typeable, catch, handle, throw)
import qualified Control.Monad.Catch as Exceptions (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable as Foldable
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Development.Shake (
  Action,
  Rules,
  ShakeValue,
  actionCatch,
  liftIO,
 )
import qualified Development.Shake as Shake (need, (%>))
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.Command (CmdArgument, IsCmdArgument (toCmdArgument))
import GHC.Generics (Generic)
import Path
import Path.IO (
  ensureDir,
  getPermissions,
  removeFile,
  setOwnerWritable,
  setPermissions,
 )
import System.IO.Error (isDoesNotExistError)
import Prelude hiding (writeFile)

-- import Development.Shake.Plus hiding (CmdOption (Env), addOracle, addOracleCache, phony, (%>))
-- import qualified Development.Shake.Plus as Shake (CmdOption (Env), (%>))

instance Binary (Path a t)
deriving instance Typeable (Path a t)

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

readFile :: Path a File -> Action Text
readFile path = do
  need [path]
  liftIO $ Text.readFile $ toFilePath path

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

writeFileBS :: MonadIO m => Path a File -> ByteString -> m ()
writeFileBS path content = liftIO $ ByteString.writeFile (toFilePath path) content

inDir :: (?workdir :: Path a Dir) => Path Rel t -> Path a t
inDir path = ?workdir </> path

deriving instance Show Abs
deriving instance Typeable Abs
deriving instance Eq Abs
deriving instance Generic Abs
instance Hashable Abs
instance Binary Abs
instance NFData Abs

deriving instance Show Rel
deriving instance Typeable Rel
deriving instance Eq Rel
deriving instance Generic Rel
instance Hashable Rel
instance Binary Rel
instance NFData Rel