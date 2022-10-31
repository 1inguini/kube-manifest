module Main (
  main,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml (encode)
import Optics (view)

import Manifest (manifests)
import Util (Manifest)

writeManifest :: Manifest -> IO ()
writeManifest manifest = case view #objects manifest of
  [] -> pure ()
  objects -> do
    let path = view #path manifest
    putStrLn $ "# writing to: " <> path
    ByteString.writeFile path $ foldl1 (\acc -> ((acc <> "---\n") <>)) $ Yaml.encode <$> objects

main :: IO ()
main =
  traverse_ writeManifest manifests