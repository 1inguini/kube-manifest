module Main (
  main,
) where

-- import Apps (apps)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml (encode)

import Manifest
import Util

-- import Utils (App (..))

-- writeApps :: App -> IO ()
-- writeApps
--   App {yamlsManifests = []} = pure ()
-- writeApps App {yamlsPath, yamlsManifests} = do
--   putStrLn $ "# writing to: " <> yamlsPath
--   -- ByteString.putStr yaml
--   ByteString.writeFile yamlsPath $ foldl1 (\acc -> ((acc <> "---\n") <>)) $ Yaml.encode <$> yamlsManifests

main :: IO ()
main = pure () -- do
-- () <$ traverse writeApps apps