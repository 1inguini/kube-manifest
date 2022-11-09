module Main (
  main,
) where

import Manifest (yamls)
import Util (Yaml, YamlType (..))
import qualified Util

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.State.Strict (execState, get)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (AsValue (_Object, _String), key, _Key)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (foldlM)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (IsString (fromString))
import Data.String.Conversions (convertString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml (decodeAllThrow, encode)
import Optics (modifying, over, preview, view, (%), _head)
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)

processYaml :: [FilePath] -> Yaml -> IO [FilePath]
processYaml written yaml =
  case view #yamlType yaml of
    Manifest ->
      let ?namespace =
            fromMaybe Util.noNamespace $
              preview (#value % key "metadata" % key "namespace" % _String) yaml <|> do
                kind <- preview (#value % key "kind" % _String) yaml
                if kind == "Namespace"
                  then preview (#value % key "metadata" % key "name" % _String) yaml
                  else Nothing
          ?app = fromMaybe "unknown-app" $ preview (#value % key "metadata" % key "labels" % key "app" % _String) yaml
       in objectWrite written $ view #value yaml
    HelmValues r -> do
      let ?namespace = view #namespace r
      let values = convertString $ Yaml.encode $ view #value yaml
      aesons <-
        readProcess
          "helm"
          [ "template"
          , "--values"
          , "-"
          , view #chart r
          ]
          values
          >>= Yaml.decodeAllThrow @IO @Aeson.Value . fromString
      let objects =
            execState
              ( do
                  modifying (key "metadata" % _Object) $
                    KeyMap.insert "namespace" $
                      Aeson.String ?namespace
                  yaml <- get
                  modifying (key "metadata" % key "labels" % _Object) $
                    KeyMap.insert "app" $
                      fromMaybe "unknown-app" $
                        preview
                          (key "metadata" % key "labels" % key (view (#appLabel % _Key) r))
                          yaml
              )
              <$> aesons
      let ?app =
            fromMaybe "unknown-app" $
              preview (_head % key "metadata" % key "labels" % key "app" % _String) objects
      valuesPath <- path "values/"
      putStrLn $ "# writing to: " <> valuesPath
      ByteString.writeFile valuesPath $ Yaml.encode $ view #value yaml
      written <- processYaml written $ Util.manifest Util.namespace
      foldlM objectWrite written objects
 where
  path dir = do
    createDirectoryIfMissing True dir
    pure $ dir <> Text.unpack ?app <> ".yaml"
  objectWrite :: (?namespace :: Text, ?app :: Text) => [FilePath] -> Aeson.Value -> IO [FilePath]
  objectWrite written object = do
    path <-
      path $
        "manifest/"
          <> Text.unpack ?namespace
          <> "/"
          <> ( case preview (key "kind") object of
                Just "PersistentVolumeClaim" -> "never-delete/"
                _ -> mempty
             )
    putStrLn $ "# writing to: " <> path
    ( if path `elem` written
        then ByteString.appendFile path . ("---\n" <>)
        else ByteString.writeFile path
      )
      $ Yaml.encode object
    pure $ path : written

generate :: IO ()
generate = void $ foldlM processYaml [] yamls

main :: IO ()
main = generate