module Util.Shake.Container (
  ContainerId,
  ContainerfileInstrution,
  Image (..),
  ImageName (..),
  ImageRepo (..),
  ImageRule (..),
  ImageTag (..),
  addContainerImageRule,
  addTaggedImageTarget,
  commit,
  copyDirPrefixed,
  dockerIo,
  from,
  image,
  latest,
  mount,
  needImage,
  needImages,
  podman,
  pushEnd,
  registry,
  runtime,
) where

import Util (Owner)
import qualified Util
import Util.Shake (copyDir, runProg, (<:>))

import Control.Exception.Safe (throwString)
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import Data.String.Conversions (cs)
import Development.Shake (
  Action,
  CmdOption,
  CmdResult,
  Exit (Exit),
  RuleResult,
  Rules,
  Stdout (Stdout),
  StdoutTrim (StdoutTrim, fromStdoutTrim),
  addTarget,
  copyFile',
  need,
  phonys,
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
import System.FilePath (dropDrive, takeFileName, (</>))
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
      runtime $ words "images --no-trunc --quiet" <> [show name]
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
type ContainerfileInstrution = String

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
runtime :: (CmdResult r, ?opts :: [CmdOption]) => [String] -> Action r
runtime = runProg . (podman :)

-- podmanCopy ::
--   (?opts :: [CmdOption], ?container :: ContainerId) =>
--   FilePath ->
--   FilePath ->
--   Action ()
-- podmanCopy tarFile dir = do
--   need [tarFile]
--   tar <- liftIO $ ByteString.Lazy.readFile tarFile
--   putInfo $ "`podman cp` from" <:> tarFile
--   let ?opts = StdinBS tar : BinaryPipes : ?opts
--   runtime @() ["cp", "--archive=false", "--overwrite=true", "-", ?container <> ":" <> dir]
--   putInfo $ "done: `podman cp` from" <:> tarFile

mount ::
  (?opts :: [CmdOption], ?container :: ContainerId) => ((?rootfs :: FilePath) => Action a) -> Action a
mount act = do
  StdoutTrim rootfs <- runtime ["mount", ?container]
  let ?rootfs = rootfs
  result <- act
  runtime @() ["unmount", ?container]
  pure result

copyDirPrefixed :: (?rootfs :: FilePath, ?owner :: Owner) => FilePath -> FilePath -> Action ()
copyDirPrefixed src dst = copyDir src $ ?rootfs </> dropDrive dst

commit ::
  ( ?opts :: [CmdOption]
  , ?imageName :: ImageName
  , ?container :: ContainerId
  ) =>
  [ContainerfileInstrution] ->
  Action ()
commit insts = do
  -- time <- iso8601Show <$> liftIO getZonedTime
  -- let labels =
  --       [ "LABEL org.opencontainers.image.created=$seconds"
  --       , "LABEL org.opencontainers.image.authors=\"1inguini <9647142@gmail.com>\""
  --       , "LABEL org.opencontainers.image.url=https://registry.1inguini.com/library/$name"
  --       , "LABEL org.opencontainers.image.url=https://hub.docker.com/repository/docker/1inguini/$name"
  --       , "LABEL org.opencontainers.image.documentation=https://git.1inguini.com/1inguini/kube-manifest/$name"
  --       , "LABEL org.opencontainers.image.documentation=https://github.com/1inguini/kube-manifest/$name"
  --       , "LABEL org.opencontainers.image.source=https://git.1inguini.com/1inguini/kube-manifest/$name"
  --       , "LABEL org.opencontainers.image.source=https://github.com/1inguini/kube-manifest/$name"
  --       ]
  runtime $
    ["commit", "--include-volumes=false"]
      <> concatMap (("--change" :) . (: [])) insts
      <> [?container, show ?imageName]

pushEnd :: (?opts :: [CmdOption], ?imageName :: ImageName, ?container :: ContainerId) => Action ()
pushEnd = do
  need ["podman/login"]
  -- runtime @() ["push", show ?imageName]
  runAfter $ do
    callProcess podman . words $ "stop --time=0" <:> ?container
    callProcess podman . words $ "rm" <:> ?container

getInsts :: (?opts :: [CmdOption], ?imageName :: ImageName) => Action [ContainerfileInstrution]
getInsts = do
  let
    inspect field =
      fromStdoutTrim
        <$> runtime
          [ "inspect"
          , "--format={{.Config." <> field <> "}}"
          , show ?imageName
          ]
    inspectArray field =
      fromStdoutTrim
        <$> runtime
          [ "inspect"
          , "--format=[{{range $index, $elem := .Config." <> field <> "}}{{if $index}}, {{end}}\"{{$elem}}\"{{end}}]"
          , show ?imageName
          ]
  user <- inspect "User"
  workdir <- inspect "WorkingDir"
  entrypoint <- inspectArray "Entrypoint"
  cmd <- inspectArray "Cmd"
  pure
    [ "USER" <:> user
    , "WORKDIR" <:> workdir
    , "ENTRYPOINT" <:> entrypoint
    , "CMD" <:> cmd
    ]

from ::
  (?opts :: [CmdOption]) =>
  ImageName ->
  [String] ->
  ((?container :: ContainerId, ?init :: String) => Action a) ->
  Action a
from base args act = withTempDir $ \tmp -> do
  let
    init = "busybox/busybox"
    cmd = takeFileName init
  copyFile' init $ tmp </> cmd
  StdoutTrim container <-
    runtime $
      ["run", "--detach", "-t", "--volume=" <> tmp <> ":/tmp", "--entrypoint=/tmp" </> cmd]
        <> args
        <> [show base, "sh"]
  let
    ?container = container
    ?init = "/tmp" </> cmd
   in
    act