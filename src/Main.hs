module Main (main) where

import Project (projects)
import Util (Helm, Project, defaultHelm, encodeAll)

import Control.Exception.Safe (throwString, try)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (_String)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (traverse_)
import Data.Record.Anon
import Data.Record.Anon.Simple (Record)
import Data.String.Conversions (cs)
import qualified Data.Yaml as Yaml
import GHC.IO.Exception (IOException (IOError))
import Optics (Lens', preview, view)
import System.Directory (createDirectoryIfMissing, removeDirectory, removeDirectoryRecursive)
import System.FilePath (dropFileName, (</>))

generate :: IO ()
generate = traverse_ processProject projects

projectDir :: FilePath
projectDir = "werf"

processProject :: Project -> IO ()
processProject proj = do
  let werf = encodeAll $ view #project proj : view #images proj
  projectName <- maybe (throwString "no field `project`") pure $ do
    proj <- KeyMap.lookup "project" $ view #project proj
    preview _String proj
  let dir = projectDir </> cs projectName
  createDirectoryIfMissing True dir
  ByteString.writeFile (dir </> "werf.yaml") werf
  putStrLn $ "# wrote to: " <> dir </> "werf.yaml"
  processHelm (dir </> ".helm") $ view #helm proj

processHelm :: FilePath -> Helm -> IO ()
processHelm dir helm = do
  createDirectoryIfMissing True dir
  let mayWrite :: Eq a => (a -> ByteString) -> Lens' Helm a -> FilePath -> IO ()
      mayWrite encode field subPath =
        let val = view field helm
         in unless (view field defaultHelm == val) $ do
              let path = dir </> subPath
              createDirectoryIfMissing True $ dropFileName path
              ByteString.writeFile path $ encode val
              putStrLn $ "# wrote to: " <> path
  mayWrite encodeAll #templates "templates/manifests.yaml"
  mayWrite encodeAll #crds "crds/crds.yaml"
  mayWrite Yaml.encode #values "values.yaml"
  mayWrite (cs . Aeson.encode) #valuesSchema "values.schema.json"
  mayWrite cs #readme "README.md"
  mayWrite cs #license "LICENSE"
  mayWrite cs #helmignore ".helmignore"

main :: IO ()
main = do
  _ <- try @_ @IOError $ removeDirectoryRecursive projectDir
  createDirectoryIfMissing True projectDir
  generate
