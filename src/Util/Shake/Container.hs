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
  dockerIo,
  image,
  latest,
  needImage,
  needImages,
  podman,
  podmanCommit,
  podmanCopy,
  podmanFrom,
  podmanPushEnd,
  registry,
  runDocker,
) where

import qualified Util
import Util.Shake (runProg, (<:>))

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
  CmdOption (BinaryPipes, StdinBS),
  CmdResult,
  Exit (Exit),
  RuleResult,
  Rules,
  Stdout (Stdout),
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
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeFileName, (</>))
import System.Process (callProcess)

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

addContainerImageRule :: (?opts :: [CmdOption]) => Rules ()
addContainerImageRule = addBuiltinRule noLint imageIdentity run
 where
  imageIdentity :: BuiltinIdentity ImageName Image
  imageIdentity _ = Just . view #id

  newStore :: ImageName -> Action (Maybe ByteString)
  newStore name = do
    (Exit exitCode, Stdout stdout) <-
      runDocker $ words "images --no-trunc --quiet" <> [show name]
    case (exitCode, ByteString.split (fromIntegral $ fromEnum '\n') stdout) of
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

podman :: String
podman = "podman"
runDocker :: (CmdResult r, ?opts :: [CmdOption]) => [String] -> Action r
runDocker = runProg . (podman :)

podmanCopy ::
  (?opts :: [CmdOption], ?container :: ContainerId) =>
  FilePath ->
  FilePath ->
  Action ()
podmanCopy tarFile dir = do
  need [tarFile]
  tar <- liftIO $ ByteString.Lazy.readFile tarFile
  putInfo $ "`podman cp` from" <:> tarFile
  let ?opts = StdinBS tar : BinaryPipes : ?opts
  runDocker @() ["cp", "--archive=false", "--overwrite=true", "-", ?container <> ":" <> dir]
  putInfo $ "done: `podman cp` from" <:> tarFile

podmanCommit ::
  ( ?opts :: [CmdOption]
  , ?imageName :: ImageName
  , ?container :: ContainerId
  ) =>
  [ContainerfileCommand] ->
  Action ()
podmanCommit commands =
  runDocker $
    ["commit", "--include-volumes=false"]
      <> concatMap (("--change" :) . (: [])) commands
      <> [?container, show ?imageName]

podmanPushEnd :: (?opts :: [CmdOption], ?imageName :: ImageName, ?container :: ContainerId) => Action ()
podmanPushEnd = do
  need ["podman/login"]
  -- runDocker @() ["push", show ?imageName]
  runAfter $ do
    callProcess podman . words $ "stop --time=0" <:> ?container
    callProcess podman . words $ "rm" <:> ?container

podmanFrom ::
  (?opts :: [CmdOption]) =>
  ImageName ->
  [String] ->
  ((?container :: ContainerId, ?init :: String) => Action a) ->
  Action a
podmanFrom base opt act = withTempDir $ \tmp -> do
  let
    init = "busybox/busybox"
    cmd = takeFileName init
  copyFile' init $ tmp </> cmd
  Stdout container <-
    runDocker $
      [ "run"
      , "--detach"
      , "-t"
      , "--volume=" <> tmp <> ":/tmp"
      , "--entrypoint=/tmp" </> cmd
      ]
        <> opt
        <> [show base, "sh"]
  let ?container = head $ lines container
      ?init = "/tmp" </> cmd
   in act