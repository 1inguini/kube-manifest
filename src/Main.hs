module Main (
  main,
) where

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
import Optics (over, view, (%))
import System.Directory (createDirectoryIfMissing)
import System.Process (readProcess)
import Util (Yaml, YamlType (..))
import qualified Util

processYaml :: [FilePath] -> Yaml -> IO [FilePath]
processYaml written yaml =
  let ?name = view #name yaml
   in case view #yamlType yaml of
        Manifest -> do
          manifestWrite (view #name yaml) (Yaml.encode $ view #value yaml)
        HelmValues r ->
          let ?namespace = view #namespace r
              ?name = view #name yaml
           in do
                valuesPath <- path "values/" ?name
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
                let ns = Yaml.encode Util.namespace
                let manifest =
                      foldl (\acc -> ((acc <> "---\n") <>)) ns $ Yaml.encode <$> objects
                manifestWrite ?name manifest
 where
  path dir name = do
    createDirectoryIfMissing True dir
    pure $ dir <> Text.unpack name <> ".yaml"
  manifestWrite ::
    Text ->
    ByteString ->
    IO [FilePath]
  manifestWrite name manifest = do
    path <- path "manifest/" name
    putStrLn $ "# writing to: " <> path
    if path `elem` written
      then ByteString.appendFile path $ "---\n" <> manifest
      else ByteString.writeFile path manifest
    pure $ path : written

main :: IO ()
main = do
  _ <- foldlM processYaml [] yamls
  pure ()
