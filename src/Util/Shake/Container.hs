module Util.Shake.Container (
  ContainerId,
  ContainerfileCommand,
  Image (..),
  ImageName (..),
  ImageRepo (..),
  ImageRule (..),
  ImageTag (..),
  addContainerImageRule,
  addTaggedImageTarget,
  docker,
  dockerCopy,
  dockerFrom,
  dockerIo,
  dockerPushEnd,
  image,
  latest,
  needImage,
  needImages,
  registry,
  runDocker,
) where

import Util.Shake (parallel_, (<:>))

import Control.Exception.Safe (throwString)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.List as List
import Data.String.Conversions (cs)
import Development.Shake (
  Action,
  RuleResult,
  Rules,
  addTarget,
  copyFile',
  need,
  phonys,
  putInfo,
  runAfter,
  withTempDir,
 )
import Development.Shake.Classes (Binary, Hashable, NFData, Typeable)
import Development.Shake.Rule (
  BuiltinIdentity,
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
import GHC.Generics (Generic)
import Optics (view)
import System.FilePath ((</>))
import System.Process.Typed (ExitCode (ExitSuccess), ProcessConfig, byteStringInput, proc, readProcessStdout, readProcessStdout_, runProcess, runProcess_, setStdin)
import qualified Util

newtype ImageRepo = ImageRepo {repo :: String}
  deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)
newtype ImageTag = ImageTag {tag :: String}
  deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)
newtype ImageName = ImageName (ImageRepo, ImageTag) -- (name, tag)
  deriving (Typeable, Eq, Hashable, Binary, NFData)
instance Show ImageName where
  show (ImageName (repo, tag)) = view #repo repo <> ":" <> view #tag tag
newtype Image = ImageId {id :: ByteString}
  deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)
newtype ImageRule = ImageRule {rule :: ImageName -> Maybe (Action ())}
type instance RuleResult ImageName = Image

addContainerImageRule :: Rules ()
addContainerImageRule = addBuiltinRule noLint imageIdentity run
 where
  imageIdentity :: BuiltinIdentity ImageName Image
  imageIdentity _ = Just . view #id

  newStore :: ImageName -> Action (Maybe ByteString)
  newStore name =
    do
      let ?proc = proc
      (exitCode, stdout) <-
        readProcessStdout . docker $
          words "images --no-trunc --quiet" <> [show name]
      case (exitCode, ByteString.split (fromIntegral $ fromEnum '\n') $ cs stdout) of
        (ExitSuccess, newStore : _) -> pure $ Just newStore
        _ -> pure Nothing

  run :: BuiltinRun ImageName Image
  run key oldStore RunDependenciesChanged = do
    (_, act) <- getUserRuleOne key (const Nothing) $ \(ImageRule act) -> act key
    act
    current <- newStore key
    case current of
      Nothing -> throwString $ "action did not create container image:" <:> show key
      Just current ->
        pure
          $ RunResult
            (if Just current == oldStore then ChangedRecomputeSame else ChangedRecomputeDiff)
            current
          $ ImageId current
  run key oldStore RunDependenciesSame = do
    current <- newStore key
    case current of
      Nothing -> run key oldStore RunDependenciesChanged
      Just current -> pure $ RunResult ChangedNothing current $ ImageId current

needImages :: [ImageName] -> Action ()
needImages = void . apply

needImage :: ImageName -> Action ()
needImage = void . apply1

type ContainerId = String
type ContainerfileCommand = String

latest :: ImageTag
latest = ImageTag "latest"

dockerIo :: String -> ImageRepo
dockerIo = ImageRepo . ("docker.io" </>)

registry :: String -> ImageRepo
registry = ImageRepo . (cs Util.registry </>)

infix 1 `image`
image :: ImageRepo -> ((?imageName :: ImageName) => Action ()) -> Rules ()
image repo act = do
  addTarget $ view #repo repo
  addTarget . show $ ImageName (repo, latest)

  phonys $ \name -> do
    colonTag <- List.stripPrefix (view #repo repo) name
    let tag = case colonTag of
          "" -> Just latest
          ':' : tag -> Just $ ImageTag tag
          _ -> Nothing
    needImage . ImageName . (,) repo <$> tag

  addUserRule . ImageRule $ \case
    name@(ImageName (r, _)) | r == repo -> Just $ let ?imageName = name in act
    _ -> Nothing

addTaggedImageTarget :: ImageName -> Rules ()
addTaggedImageTarget = addTarget . show

docker :: (?proc :: String -> [String] -> a) => [String] -> a
docker = ?proc "podman"
runDocker ::
  (MonadIO m, ?proc :: String -> [String] -> ProcessConfig stdin stdout stderr) => [String] -> m ()
runDocker = runProcess_ . docker

dockerCopy ::
  (?proc :: String -> [String] -> ProcessConfig stdin stdout stderr, ?container :: ContainerId) =>
  ByteString.Lazy.ByteString ->
  FilePath ->
  Action ()
dockerCopy tar dir = do
  -- putInfo $ "`docker cp` from" <:> tarFile
  -- need [tarFile]
  -- tar <- liftIO $ ByteString.Lazy.readFile tarFile
  let proc = ?proc
  let ?proc = \command -> setStdin (byteStringInput tar) . proc command
   in runDocker ["cp", "--archive=false", "-", ?container <> ":" <> dir]

dockerPushEnd :: (?imageName :: ImageName, ?container :: ContainerId) => Action ()
dockerPushEnd = do
  let ?proc = proc
  need ["docker/login"]
  runAfter $ do
    runDocker ["push", show ?imageName]
    runDocker . words $ "stop --time=0" <:> ?container
    runDocker . words $ "rm" <:> ?container

dockerFrom ::
  (?shakeDir :: FilePath) =>
  ImageName ->
  [String] ->
  ((?container :: ContainerId) => Action a) ->
  Action a
dockerFrom base opt act = withTempDir $ \tmp -> do
  let ?proc = proc
  let init = "busybox/busybox"
  copyFile' init $ tmp </> "sh"
  container <-
    fmap (head . lines . cs) . readProcessStdout_ . docker $
      [ "run"
      , "--detach"
      , "-t"
      , "--volume=" <> tmp <> ":/tmp"
      , "--entrypoint=/tmp/sh"
      ]
        <> opt
        <> [show base]
  let ?container = container
   in act