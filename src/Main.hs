module Main (
  main,
) where

import Manifest (yamls)
import Util (Yaml, YamlType (..), nonroot, tshow)
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

import Control.Exception.Safe (throwString)
import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import Data.Aeson.Optics (AsValue (_Object, _String), key, _Key)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy as ByteString
import Data.Either (partitionEithers)
import Data.Hashable (hash)
import Data.Record.Anon
import Data.Record.Anon.Simple (Record)
import qualified Data.Record.Anon.Simple as Anon
import Data.String.Conversions (ConvertibleStrings (convertString), convertString)
import Data.Text (Text)
import qualified Data.Text as Text
import Development.Shake (Lint (LintBasic), getShakeOptionsRules, progressSimple)
import Development.Shake.Plus
import GHC.Stack (HasCallStack)
import qualified Network.Wreq as Wreq (get, responseBody)
import Optics (modifying, over, preview, set, view, (%), _head)
import Path.IO (createDir, createDirIfMissing, doesPathExist)
import Text.Heredoc (here)

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

instance CmdResult r => CmdArguments (RAction e r) where
  cmdArguments (CmdArgument x) = case partitionEithers x of
    (opts, x : xs) -> liftAction $ command opts x xs
    _ -> error "Error, no executable or arguments given to Development.Shake.cmd"

s :: String -> String
s = id

newtype ListDynamicDep = ListDynamicDep () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ListDynamicDep = [Path Abs File]

newtype Download = Download String deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult Download = ByteString

passwd :: Text
passwd =
  Text.unlines
    [ "root:x:0:0:root:/root:/sbin/nologin"
    , "nonroot:x:" <> tshow nonroot <> ":" <> tshow nonroot <> ":nonroot:/home/nonroot:/sbin/nologin"
    ]

group :: Text
group =
  Text.unlines
    [ "root:x:0:"
    , "nonroot:x:" <> tshow nonroot
    ]

nsswitch :: Text
nsswitch =
  [here|
group:          files
gshadow:         files
passwd:         files
shadow:         files

hosts:          files dns
networks:       files

ethers:         files
protocols:      files
rpc:            files
services:       files
|]

main :: IO ()
main = shakeArgs
  shakeOptions
    { shakeThreads = 0
    , shakeLint = Just LintBasic
    , shakeColor = True
    , shakeProgress = progressSimple
    , shakeShare = Just ".shake"
    }
  $ runShakePlus ()
  $ do
    let artifactDir = [reldir|artifact|]
    shakeOptions <- liftRules getShakeOptionsRules
    shakeDir <- parseRelDir $ shakeFiles shakeOptions
    let buildDir = shakeDir </> [reldir|build|]
    let image = buildDir </> [reldir|image|]

    _ <- addOracleCache $ \(Download url) ->
      liftIO $ (^. Wreq.responseBody) <$> Wreq.get url
    let askDownload = askOracle . Download

    phony "image/scratch" $ do
      let ruleDir = buildDir </> [reldir|image/scratch|]
      cacert <-
        askDownload
          "https://ccadb-public.secure.force.com/mozilla/IncludedRootsPEMTxt?TrustBitsInclude=Websites"
      -- "container/"
      pure ()