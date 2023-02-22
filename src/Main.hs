module Main (main) where

import Project (projects)
import Util (Helm, Project, defaultHelm, encodeAll)

import Control.Exception.Safe (throwString, try)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.Aeson.Optics (key, _String)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (traverse_)
import Data.String.Conversions (cs)
import qualified Data.Yaml as Yaml
import Optics (Ixed (ix), Lens', preview, view, (%))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath (dropFileName, (<.>), (</>))
import Text.Casing (fromHumps, toKebab)

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
  _ <- try @_ @IOError $ removeDirectoryRecursive dir
  createDirectoryIfMissing True dir
  ByteString.writeFile (dir </> "werf.yaml") werf
  putStrLn $ "# wrote to: " <> dir </> "werf.yaml"
  processHelm (dir </> ".helm") $ view #helm proj

processHelm :: FilePath -> Helm -> IO ()
processHelm dir helm = do
  createDirectoryIfMissing True dir
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
      writeCrds = traverse_ $ \yaml -> do
        name <-
          maybe (throwString "CRD has no name") (pure . cs) $
            preview (ix "metadata" % key "name" % _String) yaml
        mayWrite Yaml.encode yaml $ "crds" </> name <.> "yaml"

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
  generate
