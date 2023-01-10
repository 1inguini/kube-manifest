module Main (
  main,
) where

import Manifest (yamls)
import Util (Yaml, YamlType (..), s)
import qualified Util

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (execState, get)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (AsValue (_Object, _String), key, _Key)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (foldlM)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (IsString (fromString))
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml (decodeAllThrow, encode)
import Development.Shake hiding (cmd, cmd_, command, command_)
import qualified Development.Shake as Shake
import Optics (modifying, over, preview, view, (%), _head)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import System.FilePath ((</>))
import System.Posix (CMode (CMode), setFileCreationMask)
import System.Posix.Files (setFileCreationMask)
import System.Process.Typed (proc, readProcessStdout_, runProcess_)
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
--       let values = convertString $ Yaml.encode $ view #value yaml
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

runProc :: MonadIO m => String -> m ()
runProc = runProcess_ . fromString

(<:>) :: String -> String -> String
x <:> y = x <> " " <> y

mkdir :: MonadIO m => FilePath -> m ()
mkdir = liftIO . createDirectoryIfMissing True

docker = proc "podman"

nonrootImage :: Rules ()
nonrootImage = do
  phony "nonroot" $ do
    let pause = "/usr/bin/s6-pause" -- "s6-utils/bin/s6-pause"
    need ["nonroot.tar", pause]
    container <-
      fmap (init . cs) . readProcessStdout_ . docker . words $
        "run --detach --volume=" <> pause <> ":/pause --entrypoint=/pause" <:> cs Util.registry </> "scratch"
    runProcess_ . docker . words $ "cp nonroot.tar" <:> container <> ":/"
    runProcess_ . docker . words $ "commit" <:> container <:> cs Util.registry </> "nonroot"
    runProcess_ . docker . words $ "rm" <:> container

  "nonroot.tar" %> \out -> do
    need $ ("nonroot/rootfs/etc/" </>) <$> ["passwd", "group"]
    void $
      parallel
        [ mkdir "nonroot/rootfs/tmp"
        , mkdir "nonroot/rootfs/home/nonroot"
        ]
    runProc $ "tar -c --owner=nonroot --group=nonroot -f" <:> out <:> "-C nonroot/rootfs ."

  writeFile' "nonroot/rootfs/etc/passwd" $
    unlines
      [ "root:x:0:0:root:/root:/sbin/nologin"
      , "nobody:x:65534:65534:Nobody:/nonexistent:/sbin/nologin"
      , "nonroot:x:" <> show Util.nonroot <> ":" <> show Util.nonroot <> ":nonroot:/home/nonroot:/sbin/nologin"
      ]

  writeFile' "nonroot/rootfs/etc/group" $
    unlines
      [ "root:x:0:"
      , "nobody:x:65534:"
      , "nonroot:x:" <> show Util.nonroot <> ":"
      ]

main :: IO ()
main = do
  setFileCreationMask $ CMode 0o022
  shakeArgs
    shakeOptions
      { shakeThreads = 0
      , shakeLint = Just LintBasic
      , shakeColor = True
      , shakeProgress = progressSimple
      }
    $ do
      shakeOptions <- getShakeOptionsRules
      mkdir (shakeFiles shakeOptions)
      liftIO $ setCurrentDirectory (shakeFiles shakeOptions)

      nonrootImage
