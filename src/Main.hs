module Main (
  main,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (key, _Object)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (traverse_)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml (decodeAllThrow, encode)
import Optics (over, review, view, (%))
import System.Process (callCommand, callProcess, readProcess)

import Manifest (helmValuess, manifests)
import Util (HelmValues, Manifest)
import qualified Util

writeManifest :: Manifest -> IO ()
writeManifest manifest = case view #objects manifest of
  [] -> pure ()
  objects -> do
    let path = view #path manifest
    putStrLn $ "# writing to: " <> path
    ByteString.writeFile path $ foldl1 (\acc -> ((acc <> "---\n") <>)) $ Yaml.encode <$> objects

runHelm :: HelmValues -> IO ()
runHelm values = do
  let yaml = Text.unpack (view #name values) <> ".yaml"
      valuesPath = "values/" <> yaml
      path = "manifest/" <> yaml
  putStrLn $ "# writing to: " <> valuesPath
  ByteString.writeFile valuesPath $ Yaml.encode $ view #values values
  aesons <-
    readProcess
      "helm"
      [ "template"
      , "--values"
      , valuesPath
      , view #chart values
      ]
      ""
      >>= Yaml.decodeAllThrow @IO @Aeson.Value . fromString
  let objects = over (key "metadata" % _Object) (KeyMap.insert "namespace" $ Aeson.String $ view #namespace values) <$> aesons
  putStrLn $ "# writing to: " <> path
  let ns =
        let ?namespace = view #namespace values
            ?name = view #name values
         in Yaml.encode Util.namespace
  ByteString.writeFile path $ foldl (\acc -> ((acc <> "---\n") <>)) ns $ Yaml.encode <$> objects

main :: IO ()
main = do
  traverse_ writeManifest manifests
  traverse_ runHelm helmValuess
