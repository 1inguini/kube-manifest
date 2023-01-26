module Util.Shake.Container (
  ContainerId,
  ContainerfileInstruction,
  Image (..),
  ImageName (..),
  ImageRepo (..),
  ImageRule (..),
  ImageTag (..),
  addContainerImageRule,
  addTaggedImageTarget,
  dockerCommit,
  dockerCopy,
  dockerIo,
  dockerPushEnd,
  dockerSetup,
  image,
  latest,
  needDocker,
  needDockerLogin,
  needImage,
  needImages,
  registry,
  withContainer,
) where

import qualified Util
import Util.Shake (needExe, runProg, (<:>))

import Control.Exception.Safe (throwString)
import Control.Monad (guard, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import Data.String.Conversions (cs)
import Data.Tuple.Optics (_1)
import Development.Shake (
  Action,
  CmdOption (FileStdin, StdinBS),
  Exit (Exit),
  RuleResult,
  Rules,
  Stdout (Stdout),
  StdoutTrim (StdoutTrim, fromStdoutTrim),
  addTarget,
  copyFile',
  need,
  par,
  parallel,
  phonys,
  putInfo,
  putWarn,
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
import Optics (view, (%))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath (makeRelative, splitDirectories, (</>))
import Text.Heredoc (here)

newtype ImageRepo = ImageRepo {repo :: String}
  deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)
newtype ImageTag = ImageTag {tag :: String}
  deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)
newtype ImageName = ImageName {name :: (ImageRepo, ImageTag)} -- (name, tag)
  deriving (Generic, Typeable, Eq, Hashable, Binary, NFData)
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
  newStore name = do
    docker <- needDocker
    (Exit exitCode, Stdout stdout) <-
      runProg [] . docker $ words "images --no-trunc --quiet" <> [show name]
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
type ContainerfileInstruction = String

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

dockerProgram :: String
dockerProgram = "podman"
needDocker :: Action ([String] -> [String])
needDocker = do
  docker <- needExe dockerProgram
  pure (docker :)
needDockerLogin :: String -> Action ()
needDockerLogin registry = need ["docker/login" </> registry]
dockerSetup :: Rules ()
dockerSetup = do
  addTarget $ "docker/login" </> head (splitDirectories (cs Util.registry))
  phonys $ \target -> do
    guard $ "docker/login/" `List.isPrefixOf` target
    let registry = makeRelative "docker/login" target
    Just $ do
      docker <- needDocker
      Exit noNeedPassword <- runProg [] . docker $ ["login", "--get-login", registry]
      case noNeedPassword of
        ExitFailure _ -> do
          putWarn $ "input username for `docker login" <:> registry <> "`"
          username <- cs <$> liftIO ByteString.getLine
          putWarn $ "input password for `docker login" <:> registry <> "`"
          password <- cs <$> liftIO ByteString.getLine
          runProg [StdinBS password] $
            docker ["login", "--username=" <> username, "--password-stdin", registry]
        ExitSuccess -> pure ()

dockerCopy :: (?container :: ContainerId) => FilePath -> FilePath -> Action ()
dockerCopy tarFile dir = do
  putInfo $ "`docker cp` from" <:> tarFile <:> "to" <:> dir
  need [tarFile]
  docker <- needDocker
  runProg @() [FileStdin tarFile] $
    docker ["cp", "--archive=false", "--overwrite", "-", ?container <> ":" <> dir]
  putInfo $ "done `docker cp` from" <:> tarFile <:> "to" <:> dir

dockerExport :: (?container :: ContainerId) => FilePath -> Action ()
dockerExport tarFile = do
  docker <- needDocker
  runProg @() [] $ docker ["export", "--output=" <> tarFile, ?container]

dockerImport ::
  (?imageName :: ImageName) =>
  [ContainerfileInstruction] ->
  FilePath ->
  Action ()
dockerImport insts tarFile = do
  need [tarFile]
  docker <- needDocker
  runProg [] $
    docker ["import", "--quiet"] <> concatMap (("--change" :) . (: [])) insts <> [tarFile, show ?imageName]

dockerCommit ::
  ( ?imageName :: ImageName
  , ?container :: ContainerId
  , ?instructions :: [ContainerfileInstruction]
  ) =>
  Action ()
dockerCommit = do
  docker <- needDocker
  runProg [] $
    docker ["commit", "--include-volumes=false"]
      <> concatMap (("--change" :) . (: [])) ?instructions
      <> [?container, show ?imageName]

dockerPushEnd :: (?imageName :: ImageName, ?container :: ContainerId) => Action ()
dockerPushEnd = do
  docker <- needDocker
  need ["docker/login" </> (head . splitDirectories . view (#name % _1 % #repo)) ?imageName]
  -- runDocker @() [] ["push", show ?imageName]
  runProg @() [] . docker . words $ "stop --time=0" <:> ?container
  runProg @() [] . docker . words $ "rm" <:> ?container

withContainer ::
  ImageName ->
  [String] ->
  ((?container :: ContainerId, ?instructions :: [ContainerfileInstruction]) => Action a) ->
  Action a
withContainer image opt act = withTempDir $ \tmp -> do
  docker <- needDocker
  let imageName = show image
      inspect format =
        fmap fromStdoutTrim . runProg @(StdoutTrim String) [] . docker $
          ["inspect", imageName, format]
      init = "/bin/catatonit"
  copyFile' init $ tmp </> "init"
  (StdoutTrim container, insts) <-
    par
      ( runProg [] . docker $
          [ "run"
          , "--detach"
          , "-t"
          , "--volume=" <> tmp <> ":/run"
          , "--entrypoint=/run/init"
          ]
            <> opt
            <> [imageName, "-P"]
      )
      $ parallel
        [ inspect [here|CMD [ {{range $index, $elem := .Config.Cmd}}{{if $index}}, {{end}}"{{$elem}}"{{end}} ]|]
        , inspect [here|ENTRYPOINT [ {{range $index, $elem := .Config.Entrypoint}}{{if $index}}, {{end}}"{{$elem}}"{{end}} ]|]
        , inspect [here|ENV{{range $elem := .Config.Env}}{{$elems := split $elem "="}} {{index $elems 0}}="{{join (slice $elems 1) "="}}"{{end}}|]
        , inspect [here|EXPOSE{{range $index, $elem := .Config.ExposedPorts}} {{$index}}{{end}}|]
        , inspect [here|LABEL{{range $elem := .Config.Labels}}{{$elems := split $elem "="}} "{{index $elems 0}}"="{{join (slice $elems 1) "="}}"{{end}}|]
        , inspect [here|STOPSIGNAL {{.Config.StopSignal}}|]
        , inspect [here|VOLUME [ {{range $index, $elem := .Config.Volumes}}{{if $index}}, {{end}}"{{$index}}"{{end}} ]|]
        , inspect [here|WORKDIR {{.Config.WorkingDir}}|]
        ]
  let ?instructions = insts
      ?container = container
   in act