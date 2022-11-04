module Main (
  main,
) where

import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (key, _Object)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (foldlM)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml (decodeAllThrow, encode)
import Manifest (yamls)
import Optics (over, preview, view, (%))
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)
import Util (Yaml, YamlType (..))
import qualified Util

processYaml :: [FilePath] -> Yaml -> IO [FilePath]
processYaml written yaml =
  let ?namespace = view #namespace yaml
      ?app = view #app yaml
   in case view #yamlType yaml of
        Manifest -> objectWrite written $ view #value yaml
        HelmValues r ->
          do
            valuesPath <- path "values/"
            putStrLn $ "# writing to: " <> valuesPath
            ByteString.writeFile valuesPath $ Yaml.encode $ view #value yaml
            aesons <-
              readProcess
                "helm"
                [ "template"
                , "--values"
                , valuesPath
                , view #chart r
                ]
                ""
                >>= Yaml.decodeAllThrow @IO @Aeson.Value . fromString
            let objects =
                  over
                    (key "metadata" % _Object)
                    (KeyMap.insert "namespace" $ Aeson.String ?namespace)
                    <$> aesons
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