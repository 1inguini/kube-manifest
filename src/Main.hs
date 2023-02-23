module Main (main) where

import Project (project)
import Util (Helm, Project, encodeAll)

import Control.Exception.Safe (throwString)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.Aeson.Optics (key, _String)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.String.Conversions (cs)
import qualified Data.Yaml as Yaml
import Optics (Ixed (ix), Lens', ifolded, itraverseOf_, preview, view, (%))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath (dropDrive, dropFileName, (<.>), (</>))
import Text.Casing (fromHumps, toKebab)

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
  traverse_
    ( \f ->
        let path = dir </> f
         in do
              isDir <- doesDirectoryExist path
              if isDir then removeDirectoryRecursive path else removeFile path
    )
    =<< listDirectory dir
  let mayWrite ::
        forall a. (Eq a, Monoid a) => (a -> ByteString) -> a -> FilePath -> IO ()
      mayWrite encode val subPath =
        let path = dir </> subPath
         in unless ((mempty :: a) == val) $ do
              createDirectoryIfMissing True $ dropFileName path
              ByteString.writeFile path $ encode val
              putStrLn $ "# wrote to: " <> path
      writeField :: (Eq a, Monoid a) => (a -> ByteString) -> Lens' Helm a -> FilePath -> IO ()
      writeField encode field = mayWrite encode (view field helm)
      writeTemplates :: [Yaml.Object] -> IO ()
      writeTemplates = traverse_ $ \yaml -> do
        name <-
          maybe (throwString "manifest has no name") (pure . cs) $
            preview (ix "metadata" % key "name" % _String) yaml
        kind <-
          maybe
            (throwString $ "manifest `" <> name <> "` has no kind")
            (pure . toKebab . fromHumps . cs)
            $ preview (ix "kind" % _String) yaml
        mayWrite Yaml.encode yaml $ "templates" </> name </> kind <.> "yaml"
      writeCrds :: [Yaml.Object] -> IO ()
      writeCrds = traverse_ $ \crd -> do
        name <-
          maybe (throwString "CRD has no name") (pure . cs) $
            preview (ix "metadata" % key "name" % _String) crd
        mayWrite Yaml.encode crd $ "crds" </> name <.> "yaml"

  writeTemplates $ view #templates helm
  writeCrds $ view #crds helm
  writeField Yaml.encode #values "values.yaml"
  writeField Yaml.encode #chart "Chart.yaml"
  writeField (cs . Aeson.encode) #valuesSchema "values.schema.json"
  writeField cs #readme "README.md"
  writeField cs #license "LICENSE"
  writeField cs #helmignore ".helmignore"

main :: IO ()
main = do
  createDirectoryIfMissing True projectDir
  processProject project

-- createDirectoryIfMissing True projectDir
-- generate
