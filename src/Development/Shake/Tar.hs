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

newtype TarEntry tag t = TarEntry (tag, Path Rel t)
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult (TarEntry tag t) = Tar.Entry

-- newtype TarEntry t c = TarEntry (Path Rel t, c) deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
-- type instance RuleResult (TarEntry File ByteString) = Tar.Entry
-- type instance RuleResult (TarEntry Dir ()) = Tar.Entry

nonrootOwn :: Ownership
nonrootOwn =
  Ownership
    { ownerName = "nonroot"
    , groupName = "nonroot"
    , ownerId = Util.nonroot
    , groupId = Util.nonroot
    }

fileEntry :: MonadThrow m => Path Rel File -> ByteString -> m Tar.Entry
fileEntry path content = do
  path <- either throwString pure . Tar.toTarPath False . cs $ toFilePath path
  pure (Tar.fileEntry path $ cs content){entryOwnership = nonrootOwn}

directoryEntry :: MonadThrow m => Path Rel Dir -> m Tar.Entry
directoryEntry path = do
  path <- either throwString pure . Tar.toTarPath True . cs $ toFilePath path
  pure (Tar.directoryEntry path){entryOwnership = nonrootOwn}

-- addFileEntry :: forall path. Typeable path => Proxy (path :: Path Rel File) -> ByteString -> Rules ()
-- addFileEntry _ content =
--   void . addOracleCache $ \(FileEntry _ :: FileEntry path content) -> fileEntry path content

-- type instance RuleResult (TarEntry t) = Tar.Entry

newtype FileRule = FileRule (Path Rel File -> Maybe (Action ByteString))

addTarRule :: forall a. Proxy a -> ShakeValue a => Rules ()
addTarRule _ = (addBuiltinRule @(TarEntry a File) @Tar.Entry) (noLint @(TarEntry a File) @Tar.Entry) identity run
 where
  identity :: BuiltinIdentity (TarEntry a File) Tar.Entry
  identity _ entry = Just . Lazy.toStrict $ Tar.write [entry]

  run :: BuiltinRun (TarEntry a File) Tar.Entry
  run (TarEntry (_, key)) oldStore RunDependenciesChanged = do
    (_, act) <- getUserRuleOne key (const Nothing) $ \(FileRule act) -> act key
    content <- act
    RunResult
      (if Just content == oldStore then ChangedRecomputeSame else ChangedRecomputeDiff)
      content
      <$> fileEntry key content
  run (TarEntry (_, key)) (Just oldStore) RunDependenciesSame =
    RunResult ChangedNothing oldStore <$> fileEntry key oldStore
  run key Nothing RunDependenciesSame =
    run key Nothing RunDependenciesChanged

needFileEntries :: ShakeValue a => a -> [Path Rel File] -> Action [Tar.Entry]
needFileEntries tag = apply . fmap (curry TarEntry tag)

needFileEntry :: ShakeValue a => a -> Path Rel File -> Action Tar.Entry
needFileEntry tag = apply1 . curry TarEntry tag

addFileEntry path content = do
  addUserRule $
    FileRule
      ( \p ->
          if path == p
            then Just $ pure content
            else Nothing
      )

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
