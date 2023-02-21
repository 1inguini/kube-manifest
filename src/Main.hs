module Main (main) where

import Project (project)
import Util (Helm, Project, encodeAll)

import Control.Exception.Safe (throwString, try)
import Control.Monad (unless, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (_String)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (traverse_)
import Data.String.Conversions (cs)
import qualified Data.Yaml as Yaml
import Optics (Lens', preview, view)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath (dropFileName, (</>))

-- generate :: IO ()
-- generate = traverse_ processProject projects

projectDir :: FilePath
projectDir = "werf"

processProject :: Project -> IO ()
processProject proj = do
  let werf = encodeAll $ view #project proj : view #images proj
  -- projectName <- maybe (throwString "no field `project`") pure $ do
  --   proj <- KeyMap.lookup "project" $ view #project proj
  --   preview _String proj
  -- let dir = projectDir </> cs projectName
  let dir = projectDir
  createDirectoryIfMissing True dir
  let path = dir </> "werf.yaml"
  removeFile path
  ByteString.writeFile path werf
  putStrLn $ "# wrote to: " <> path
  processHelm (dir </> ".helm") $ view #helm proj

processHelm :: FilePath -> Helm -> IO ()
processHelm dir helm = do
  createDirectoryIfMissing True dir
  let mayWrite ::
        forall a. (Eq a, Monoid a) => (a -> ByteString) -> Lens' Helm a -> FilePath -> IO ()
      mayWrite encode field subPath =
        let val = view field helm
            path = dir </> subPath
            subdir = dropFileName path
         in unless ((mempty :: a) == val) $ do
              createDirectoryIfMissing True subdir
              traverse_
                ( \f ->
                    let path = subdir </> f
                     in do
                          isFile <- doesFileExist path
                          when isFile $ removeFile path
                )
                =<< listDirectory subdir
              ByteString.writeFile path $ encode val
              putStrLn $ "# wrote to: " <> path
  mayWrite encodeAll #templates "templates/manifests.yaml"
  mayWrite encodeAll #crds "crds/crds.yaml"
  mayWrite Yaml.encode #values "values.yaml"
  mayWrite Yaml.encode #chart "Chart.yaml"
  mayWrite (cs . Aeson.encode) #valuesSchema "values.schema.json"
  mayWrite cs #readme "README.md"
  mayWrite cs #license "LICENSE"
  mayWrite cs #helmignore ".helmignore"

main :: IO ()
main = do
  createDirectoryIfMissing True projectDir
  processProject project

-- createDirectoryIfMissing True projectDir
-- generate
