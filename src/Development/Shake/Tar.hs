module Development.Shake.Tar where

import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry (Entry (entryOwnership), Ownership (Ownership, ownerName))
import qualified Codec.Archive.Tar.Entry as Tar
import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy
import Data.Hashable (Hashable (hashWithSalt), hash)
import Data.Proxy (Proxy (Proxy))
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Development.Shake (
  Action,
  RuleResult,
  Rules,
  ShakeValue,
  addOracleCache,
  askOracle,
 )
import Development.Shake.Classes (Binary (get, put), NFData, Typeable)
import Development.Shake.Rule (
  BuiltinIdentity,
  BuiltinLint,
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

import Development.Shake.Util ()
import qualified Util as Util

instance Binary Tar.Entry where
  put entry = put $ Tar.write [entry]
  get = do
    entries <- Tar.read <$> get
    case entries of
      Tar.Next entry _ -> pure entry
      e -> fail $ show e
instance Hashable Tar.Entry where
  hashWithSalt salt entry = hashWithSalt salt $ Tar.write [entry]

newtype TarEntry path a t = TarEntry (path, Path a t)
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult (TarEntry path a t) = Tar.Entry

-- newtype TarEntry t c = TarEntry (Path Rel t, c) deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
-- type instance RuleResult (TarEntry File ByteString) = Tar.Entry
-- type instance RuleResult (TarEntry Dir ()) = Tar.Entry

rootOwn :: Ownership
rootOwn =
  Ownership
    { ownerName = "root"
    , groupName = "root"
    , ownerId = 0
    , groupId = 0
    }

nonrootOwn :: Ownership
nonrootOwn =
  Ownership
    { ownerName = "nonroot"
    , groupName = "nonroot"
    , ownerId = Util.nonroot
    , groupId = Util.nonroot
    }

fileEntry :: MonadThrow m => Path a File -> ByteString -> m Tar.Entry
fileEntry path content = do
  path <- either throwString pure . Tar.toTarPath False . cs $ toFilePath path
  pure (Tar.fileEntry path $ cs content){entryOwnership = nonrootOwn}

-- addFileEntry :: forall path. Typeable path => Proxy (path :: Path Rel File) -> ByteString -> Rules ()
-- addFileEntry _ content =
--   void . addOracleCache $ \(FileEntry _ :: FileEntry path content) -> fileEntry path content

-- type instance RuleResult (TarEntry t) = Tar.Entry

newtype FileRule tag a = FileRule (tag, Path a File -> Maybe (Action ByteString))

addTarFileRule :: forall tag. ShakeValue tag => Rules ()
addTarFileRule = do
  (addBuiltinRule @(TarEntry tag Abs File)) (noLint @(TarEntry tag Abs File)) identity run
  (addBuiltinRule @(TarEntry tag Rel File)) (noLint @(TarEntry tag Rel File)) identity run
 where
  identity :: ShakeValue a => BuiltinIdentity (TarEntry tag a File) Tar.Entry
  identity _ entry = Just . Lazy.toStrict $ Tar.write [entry]

  run :: ShakeValue a => BuiltinRun (TarEntry tag a File) Tar.Entry
  run (TarEntry (_, key)) oldStore RunDependenciesChanged = do
    (_, act) <- getUserRuleOne key (const Nothing) $ \(FileRule (_, act) :: FileRule tag a) -> act key
    content <- act
    RunResult
      (if Just content == oldStore then ChangedRecomputeSame else ChangedRecomputeDiff)
      content
      <$> fileEntry key content
  run (TarEntry (_, key)) (Just oldStore) RunDependenciesSame =
    RunResult ChangedNothing oldStore <$> fileEntry key oldStore
  run key Nothing RunDependenciesSame =
    run key Nothing RunDependenciesChanged

needFileEntry :: (ShakeValue tag, ShakeValue a, ?tag :: tag) => Path a File -> Action Tar.Entry
needFileEntry = apply1 . curry TarEntry ?tag

addFileEntry :: (ShakeValue tag, ShakeValue a, ?tag :: tag) => Path a File -> ByteString -> Rules ()
addFileEntry path content =
  addUserRule $ FileRule (?tag, \p -> if path == p then Just $ pure content else Nothing)

addFileEntryLines :: (ShakeValue tag, ShakeValue a, ?tag :: tag) => Path a File -> [Text] -> Rules ()
addFileEntryLines path = addFileEntry path . cs . Text.unlines

needDirEntry :: MonadThrow m => Path a Dir -> m Tar.Entry
needDirEntry path = do
  path <- either throwString pure . Tar.toTarPath True . cs $ toFilePath path
  pure (Tar.directoryEntry path){entryOwnership = nonrootOwn}

-- needFileEntry :: Path Rel File -> Action Tar.Entry
-- needFileEntry path = askOracle $ FileEntry path

-- addDirEntry :: forall a. Path a File -> Path Rel Dir -> Rules ()
-- addDirEntry tar path =
--   void . addOracleCache $ \(TarEntry () :: TarEntry a tar Dir path) -> directoryEntry path

-- addTarEntryRule :: Rules ()
-- addTarEntryRule = do
--   void . addOracleCache $ \(TarEntry (path, content)) -> fileEntry path content
--   void . addOracleCache $ \(TarEntry (path, ())) -> directoryEntry path

-- needEntry :: forall a t. Path a File -> Path Rel t -> Action Tar.Entry
-- needEntry tar path =
--   let pa :: Proxy a
--       pa = Proxy
--       pt :: Proxy t
--       pt = Proxy
--    in askOracle (TarEntry () :: TarEntry pa tar pt path)
