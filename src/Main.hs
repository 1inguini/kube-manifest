{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move brackets to avoid $" #-}
module Main (
  main,
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (key, _Object)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (foldlM, traverse_)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml (decodeAllThrow, encode)
import Optics (over, review, view, (%))
import System.Directory (createDirectoryIfMissing)
import System.Process (callCommand, callProcess, readProcess)

import Manifest (yamls)
import Util (Yaml, YamlType (..))
import qualified Util

processYaml :: [FilePath] -> Yaml -> IO [FilePath]
processYaml written yaml =
  let ?name = view #name yaml
   in case view #yamlType yaml of
        ManifestNoNamespace -> do
          manifestWrite Nothing $ view #name yaml
        Manifest r -> do
          manifestWrite (Just r) $ view #name yaml
        HelmValues r ->
          let ?namespace = view #namespace r
              ?name = view #name yaml
           in do
                valuesPath <- path "values/" (Just r) ?name
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
                path <- path "manifest/" (Just r) ?name
                putStrLn $ "# writing to: " <> path
                let ns = Yaml.encode Util.namespace
                let manifest =
                      foldl (\acc -> ((acc <> "---\n") <>)) ns $ Yaml.encode <$> objects
                ByteString.writeFile path manifest
                pure $ path : written
 where
  path prefix mr name = do
    let dir = Text.unpack $ prefix <> maybe mempty ((<> "/") . view #namespace) mr
    createDirectoryIfMissing True dir
    pure $ dir <> Text.unpack name <> ".yaml"
  manifestWrite mr name = do
    path <- path "manifest/" mr name
    putStrLn $ "# writing to: " <> path
    if path `elem` written
      then ByteString.appendFile path $ "---\n" <> Yaml.encode (view #value yaml)
      else ByteString.writeFile path $ Yaml.encode (view #value yaml)
    pure $ path : written

main :: IO ()
main = do
  _ <- foldlM processYaml [] yamls
  pure ()

-- traverse_ writeManifest manifests
-- traverse_ runHelm helmValuess
