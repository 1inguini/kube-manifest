module Main (
  main,
) where

import Prelude hiding (readFile, writeFile)

import Development.Shake.Container as Container
import Development.Shake.Tar as Tar
import Development.Shake.Util
import Manifest (yamls)
import Util (Yaml, YamlType (..), s, tshow)
import qualified Util

-- import Control.Applicative ((<|>))
-- import Control.Monad (void)
-- import Control.Monad.State.Strict (execState, get)
-- import qualified Data.Aeson.KeyMap as KeyMap
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString as ByteString
-- import Data.Foldable (foldlM)
-- import Data.Maybe (catMaybes, fromMaybe)
-- import Data.String (IsString (fromString))
-- import qualified Data.Yaml as Yaml (decodeAllThrow, encode)
-- import System.Directory (createDirectoryIfMissing)
-- import System.Process (readProcess)

import Control.Exception.Safe (handle, throwString)
import Control.Lens ((^.))
import Control.Monad (foldM, unless, void, when)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import qualified Data.Aeson as Aeson
import Data.Aeson.Optics (AsValue (_Object, _String), key, _Key)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Foldable (concatMap, traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.List as List
import Data.Record.Anon
import Data.Record.Anon.Simple (Record)
import qualified Data.Record.Anon.Simple as Anon
import Data.String.Conversions (ConvertibleStrings, cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.These (These (That, These, This))
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
  parallel,
  phony,
  phonys,
  progressSimple,
 )
import Development.Shake.Command (CmdArgument (CmdArgument), IsCmdArgument (toCmdArgument))
import Development.Shake.Plus hiding (CmdOption (Env), addOracle, addOracleCache, copyFile, need, needIn, parallel, phony, (%>))
import qualified Development.Shake.Plus as Shake (CmdOption (Env), (%>))
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
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import qualified Network.Wreq as Wreq (get, responseBody)
import Optics (modifying, over, preview, set, view, (%), _head)
import Path
import Path.IO (createDir, createDirIfMissing, doesPathExist, ensureDir)
import qualified Secret as Util
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Text.Heredoc (here, str)

-- processYaml :: [FilePath] -> Yaml -> IO [FilePath]
-- processYaml written yaml =
--   case view #yamlType yaml of
--     Manifest ->
--       let ?namespace =
--             fromMaybe Util.noNamespace $
--               preview (#value % key "metadata" % key "namespace" % _String) yaml <|> do
--                 kind <- preview (#value % key "kind" % _String) yaml
--                 if kind == "Namespace"
--                   then preview (#value % key "metadata" % key "name" % _String) yaml
--                   else Nothing
--           ?app = fromMaybe "unknown-app" $ preview (#value % key "metadata" % key "labels" % key "app" % _String) yaml
--        in objectWrite written $ view #value yaml
--     HelmValues r -> do
--       let ?namespace = view #namespace r
--       let values = cs $ Yaml.encode $ view #value yaml
--       aesons <-
--         readProcess
--           "helm"
--           [ "template"
--           , "--values"
--           , "-"
--           , view #chart r
--           ]
--           values
--           >>= Yaml.decodeAllThrow @IO @Aeson.Value . fromString
--       let objects =
--             execState
--               ( do
--                   modifying (key "metadata" % _Object) $
--                     KeyMap.insert "namespace" $
--                       Aeson.String ?namespace
--                   yaml <- get
--                   modifying (key "metadata" % key "labels" % _Object) $
--                     KeyMap.insert "app" $
--                       fromMaybe "unknown-app" $
--                         preview
--                           (key "metadata" % key "labels" % key (view (#appLabel % _Key) r))
--                           yaml
--               )
--               <$> aesons
--       let ?app =
--             fromMaybe "unknown-app" $
--               preview (_head % key "metadata" % key "labels" % key "app" % _String) objects
--       valuesPath <- path "values/"
--       putStrLn $ "# writing to: " <> valuesPath
--       ByteString.writeFile valuesPath $ Yaml.encode $ view #value yaml
--       written <- processYaml written $ Util.manifest Util.namespace
--       foldlM objectWrite written objects
--  where
--   path dir = do
--     createDirectoryIfMissing True dir
--     pure $ dir <> Text.unpack ?app <> ".yaml"
--   objectWrite :: (?namespace :: Text, ?app :: Text) => [FilePath] -> Aeson.Value -> IO [FilePath]
--   objectWrite written object = do
--     path <-
--       path $
--         "manifest/"
--           <> Text.unpack ?namespace
--           <> "/"
--           <> ( let neverDelete = "never-delete/"
--                 in case preview (key "kind") object of
--                     Just "PersistentVolumeClaim" -> neverDelete
--                     Just "Namespace" -> neverDelete
--                     _ -> mempty
--              )
--     putStrLn $ "# writing to: " <> path
--     ( if path `elem` written
--         then ByteString.appendFile path . ("---\n" <>)
--         else ByteString.writeFile path
--       )
--       $ Yaml.encode object
--     pure $ path : written

-- generate :: IO ()
-- generate = void $ foldlM processYaml [] yamls

-- main :: IO ()
-- main = generate

instance CmdResult r => CmdArguments (RAction e r) where
  cmdArguments (CmdArgument x) = case partitionEithers x of
    (opts, x : xs) -> liftAction $ command opts x xs
    _ -> error "Error, no executable or arguments given to Development.Shake.cmd"

-- instance IsCmdArgument Text where
--   toCmdArgument = toCmdArgument . (cs :: Text -> String)

-- instance IsCmdArgument [Text] where
--   toCmdArgument = toCmdArgument . fmap (cs :: Text -> String)

-- instance IsCmdArgument ByteString where
--   toCmdArgument = toCmdArgument . (cs :: ByteString -> String)

-- instance IsCmdArgument [ByteString] where
--   toCmdArgument = toCmdArgument . fmap (cs :: ByteString -> String)

pacman, aur :: CmdArgument
aur = cmd $ s "yay --noconfirm --noprovides"
pacman = cmd $ s "pacman --noconfirm" :: CmdArgument

newtype ListDynamicDep = ListDynamicDep () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ListDynamicDep = [Path Abs File]

newtype Download = Download String deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult Download = ByteString

addDownloadRule :: Rules ()
addDownloadRule = void . addOracleCache $ \(Download url) ->
  liftIO $ ByteString.toStrict . (^. Wreq.responseBody) <$> Wreq.get url

needDownload :: MonadAction m => String -> m ByteString
needDownload = askOracle . Download

buildDir :: (?shakedir :: Path a Dir) => Path a Dir
buildDir = ?shakedir </> [reldir|build|]

imageRules :: (?shakedir :: Path b Dir) => Rules ()
imageRules = do
  let ?workdir = buildDir </> [reldir|image|]
  nonrootImage
  archlinuxImage

nonrootImage :: (?workdir :: Path b Dir) => Rules ()
nonrootImage = do
  let ?workdir = ?workdir </> [reldir|nonroot|]
  let etc = ?workdir </> [reldir|etc|]
  let ?workdir = etc
   in do
        writeFileLinesIn
          [relfile|passwd|]
          [ "root:x:0:0:root:/root:/sbin/nologin"
          , "nobody:x:65534:65534:Nobody:/nonexistent:/sbin/nologin"
          , "nonroot:x:" <> tshow Util.nonroot <> ":" <> tshow Util.nonroot <> ":nonroot:/home/nonroot:/sbin/nologin"
          ]
        writeFileLinesIn
          [relfile|group|]
          [ "root:x:0:"
          , "nobody:x:65534:"
          , "nonroot:x:" <> tshow Util.nonroot <> ":"
          ]

  (nonroot `imageRuleFrom` scratch)
    [ Workdir "/home/nonroot"
    , User "nonroot"
    , description "scratch with nonroot user"
    ]
    $ do
      let ?workdir = etc
       in needIn
            [ [relfile|passwd|]
            , [relfile|group|]
            ]
      parallel $
        ensureDir . (?workdir </>)
          <$> [ [reldir|tmp|]
              , [reldir|home/nonroot|]
              ]
      copy ?workdir

archlinuxImage :: (?workdir :: Path b Dir) => Rules ()
archlinuxImage = do
  let ?workdir = ?workdir </> [reldir|archlinux|]

  writeFileLinesIn [relfile|etc/locale.gen|] ["en_US.UTF-8 UTF-8", "ja_JP.UTF-8 UTF-8"]

  (registry </> [relfile|archlinux|] `imageRuleArbitaryTagsFrom` Image ([relfile|docker.io/library/archlinux|], Tag "base-devel"))
    [ Workdir "/home/nonroot"
    , User "nonroot"
    , description "Arch Linux with nonroot user and aur helper"
    ]
    $ do
      let nonroot = show Util.nonroot
      void $
        parallel
          [ do
              rootRun_ $ cmd (s "groupadd --gid") nonroot (s "nonroot")
              rootRun_ $ cmd (s "useradd --uid") nonroot (s "--gid") nonroot (s "-m -s /usr/bin/nologin nonroot")
          , rootRun_ $
              cmd
                (Stdin "nonroot ALL=(ALL:ALL) NOPASSWD: ALL")
                (s "tee -a /etc/sudoers")
          , let noExtract =
                  [str|NoExtract  = etc/systemd/*
                      |NoExtract  = usr/share/systemd/*
                      |NoExtract  = usr/share/man/*
                      |NoExtract  = usr/share/help/*
                      |NoExtract  = usr/share/doc/*
                      |NoExtract  = usr/share/gtk-doc/*
                      |NoExtract  = usr/share/info/*
                      |NoExtract  = usr/share/X11/*
                      |NoExtract  = usr/share/systemd/*
                      |NoExtract  = usr/share/bash-completion/*
                      |NoExtract  = usr/share/fish/*
                      |NoExtract  = usr/share/zsh/*
                      |NoExtract  = usr/lib/systemd/*
                      |NoExtract  = usr/lib/sysusers.d/*
                      |NoExtract  = usr/lib/tmpfiles.d/*
                      |]
             in do
                  rootRun_ $ cmd (s "sed -i /etc/pacman.conf -e") [s "/^NoExtract/d"]
                  rootRun_ $ cmd (Stdin noExtract) (s "tee -a /etc/pacman.conf")
          , Container.copyFile
              [relfile|container/builder/mirrorlist|]
              [absfile|/etc/pacman.d/mirrorlist|]
          ]
      rootRun_ $ cmd pacman (s "-Sy git glibc moreutils rsync")
      void $
        parallel
          [ do
              run_ $
                cmd
                  (Cwd "/home/nonroot")
                  (s "git clone https://aur.archlinux.org/yay-bin.git aur-helper")
              run_ $ cmd (Cwd "/home/nonroot/aur-helper") (s "makepkg --noconfirm -sir")
          , do
              copyFile (?workdir </> [relfile|etc/locale.gen|]) [absfile|/etc/locale.gen|]
              rootRun_ . cmd $ s "locale-gen"
          ]

main :: IO ()
main = shakeArgs
  shakeOptions
    { shakeThreads = 0
    , shakeLint = Just LintBasic
    , shakeColor = True
    , shakeProgress = progressSimple
    }
  $ do
    shakedir <- parseRelDir . shakeFiles =<< getShakeOptionsRules
    let ?shakedir = shakedir
        ?artifactDir = [reldir|artifact|]
     in do
          addDownloadRule
          addContainerImageRule

          imageRules
