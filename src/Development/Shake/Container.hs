{-# LANGUAGE ExtendedDefaultRules #-}

module Development.Shake.Container where

import Control.Exception.Safe (throwString)
import Control.Monad (void, when)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Either (partitionEithers)
import qualified Data.List as List
import Data.String.Conversions (ConvertibleStrings, cs)
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Development.Shake (
  Action,
  CmdOption (Cwd),
  CmdResult,
  Exit (Exit),
  RuleResult,
  Rules,
  Stdout (Stdout),
  cmd,
  cmd_,
  liftIO,
  phony,
  phonys,
 )
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.Command (CmdArgument (CmdArgument))
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
import Development.Shake.Util ()
import GHC.Generics (Generic)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Path
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

import Secret (host)
import qualified Util

data ImageConfig
  = Cmd [String]
  | Entrypoint [String]
  | Env String String
  | Expose String
  | Label String String
  | User String
  | Volume String
  | Workdir String
  deriving (Show, Eq, Generic)

toChange :: ImageConfig -> String
toChange (Cmd cmds) = "CMD " <> show cmds
toChange (Entrypoint cmds) = "ENTRYPOINT " <> show cmds
toChange (Env var val) = "ENV " <> var <> "=" <> val
toChange (Expose port) = "EXPOSE " <> port
toChange (Label label val) = "LABEL " <> label <> "=" <> val
toChange (User user) = "USER " <> user
toChange (Volume dir) = "VOLUME " <> dir
toChange (Workdir dir) = "WORKDIR " <> dir

toArgs :: ImageConfig -> [String]
toArgs (Cmd cmds) = "--cmd" : cmds
toArgs (Entrypoint cmds) = "--entrypoint" : cmds
toArgs (Env var val) = ["--env", var, val]
toArgs (Expose port) = ["--port", port]
toArgs (Label label val) = ["--label", label <> "=" <> val]
toArgs (User user) = ["--user", user]
toArgs (Volume dir) = ["--volume", dir]
toArgs (Workdir dir) = ["--workingdir", dir]

buildah :: CmdArgument
buildah = cmd "buildah"
from :: Image -> Image -> ((?container :: ContainerId) => Action ()) -> Action ()
from image base act = do
  Stdout container <- cmd buildah "from" $ imageName base
  container <- ContainerId <$> singleLine container
  labels <- labels image
  let ?container = container in act *> commit image
copy :: (?container :: ContainerId) => Path b Dir -> Action ()
copy path =
  let ContainerId container = ?container
   in cmd_ buildah ["copy", "--chown=" <> show Util.nonroot, "--chmod=755"] container (toFilePath path) "/"
runBy :: (?container :: ContainerId) => CmdResult r => String -> CmdArgument -> Action r
runBy user (CmdArgument args) =
  let (opts, commands) = partitionEithers args
      (buildahOpts, execOpts) =
        foldl
          ( \(buildahOpts, execOpts) ->
              \case
                (Cwd cwd) -> (buildahOpts, ("--workdir=" <> cwd) : execOpts)
                opt -> (opt : buildahOpts, execOpts)
          )
          ([], [])
          opts
      ContainerId container = ?container
   in cmd buildahOpts buildah ("run --user=" <> user) execOpts container commands
run, rootRun :: (?container :: ContainerId) => CmdResult r => CmdArgument -> Action r
run = runBy "nonroot"
rootRun = runBy "root"
run_, rootRun_ :: (?container :: ContainerId) => CmdArgument -> Action ()
run_ = run
rootRun_ = rootRun
config :: (?container :: ContainerId) => [ImageConfig] -> Action ()
config config =
  let ContainerId container = ?container
   in cmd_ buildah "config" (concatMap toArgs config) container
commit :: (?container :: ContainerId) => Image -> Action ()
commit image =
  let ContainerId container = ?container
   in cmd_ buildah "commit --rm" container $ imageName image
labels :: Image -> Action [ImageConfig]
labels image = do
  dateTime <- getUTCTime
  pure $
    (\(l, v) -> Label ("org.opencontainers.image." <> l) v)
      <$> [ ("created", dateTime)
          , ("authors", "1inguini <9647142@gmail.com>")
          , ("url", cs (imageName image))
          , ("documentation", "https://git.1inguini.com/1inguini/kube-manifest/README.md")
          , ("source", "https://git.1inguini.com/1inguini/kube-manifest")
          ]
 where
  getUTCTime :: Action String
  getUTCTime = liftIO $ iso8601Show <$> getCurrentTime

description :: String -> ImageConfig
description = Label "org.opencontainers.image.description"

-- https://stackoverflow.com/q/54050016
newtype Tag = Tag String
  deriving (Show, Eq, Hashable, Binary, NFData)

latest :: Tag
latest = Tag "latest"

newtype Image = Image (Path Rel File, Tag) -- repo/name, tag
  deriving (Show, Eq, Hashable, Binary, NFData)

imageName (Image (name, Tag tag)) =
  toFilePath name <> ":" <> tag

registry :: Path Rel Dir
registry = $(TH.lift =<< TH.runIO ((</> [reldir|library|]) <$> parseRelDir ("registry." <> cs host)))

registryImage :: Path Rel File -> Tag -> Image
registryImage name = curry Image (registry </> name)
scratch, nonroot :: Image
scratch = registryImage [relfile|scratch|] latest
nonroot = registryImage [relfile|nonroot|] latest

newtype ImageHash = ImageHash ByteString
  deriving (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult Image = ImageHash -- image id (sha256)

newtype ImageRule = ImageRule (Image -> Maybe (Action ()))

needImages :: [Image] -> Action ()
needImages = void . apply

needImage :: Image -> Action ()
needImage = void . apply1

newtype ContainerId = ContainerId String
  deriving (Show, Eq, Hashable, Binary, NFData)

singleLine :: (MonadThrow m, ConvertibleStrings String s) => String -> m s
singleLine str = case lines str of
  [line] -> pure $ cs line
  [] -> throwString "no lines"
  lines -> throwString $ "multiple lines" <> show lines

addContainerImageRule :: Rules ()
addContainerImageRule = do
  addBuiltinRule noLint imageIdentity run
 where
  imageIdentity _ (ImageHash hash) = Just hash

  imageSha image = do
    (Stdout out, Exit code) <- cmd buildah "images --no-trunc --quiet" $ imageName image
    case code of
      ExitSuccess -> singleLine out
      ExitFailure 125 -> pure ""

  run :: BuiltinRun Image ImageHash
  run key oldStore RunDependenciesChanged = do
    (_, act) <- getUserRuleOne key (const Nothing) $ \(ImageRule act) -> act key
    act
    current <- imageSha key
    pure
      $ RunResult
        (if Just current == oldStore then ChangedRecomputeSame else ChangedRecomputeDiff)
        current
      $ ImageHash current
  run key oldStore RunDependenciesSame = do
    current <- imageSha key
    if ByteString.null current
      then run key oldStore RunDependenciesChanged
      else pure $ RunResult ChangedNothing current $ ImageHash current

infix 4 `imageRuleFrom`
infix 4 `imageRuleArbitaryTagsFrom`

imageRuleFrom :: Image -> Image -> ((?container :: ContainerId) => Action ()) -> Rules ()
imageRuleFrom image@(Image (name, tag)) base action = do
  phony (imageName image) $ needImage image
  when (tag == latest) $ phony (toFilePath name) $ needImage image
  addUserRule $
    ImageRule
      ( \i ->
          if image == i
            then Just $ (image `from` base) action
            else Nothing
      )

imageRuleArbitaryTagsFrom :: Path Rel File -> Image -> [ImageConfig] -> ((?container :: ContainerId) => Action ()) -> Rules ()
imageRuleArbitaryTagsFrom name base confs act = do
  phonys $ \image -> do
    colTag <- List.stripPrefix (toFilePath name) image
    tag <- case colTag of
      "" -> pure "latest"
      ':' : tag -> pure tag
      _ -> Nothing
    pure $ needImage $ Image (name, Tag tag)

  addUserRule . ImageRule $ \case
    image@(Image (n, _)) | n == name -> Just $ (image `from` base) act
    _ -> Nothing
