module Main (main) where

import Manifest (projects)
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

-- processYaml :: [FilePath] -> Yaml -> IO [FilePath]
-- processYaml written yaml =
--   case view #yamlType yaml of
--     Manifest ->
--       let ?namespace =
--             fromMaybe Util.noNamespace $
--               preview (#value % key "metadata" % key "namespace" % _String) yaml <|> do
--                 kind <- preview (#value % key "kind" % _String) yaml
--                 if kind == "Namespace"
--                   then preview (#value % key "metadata" % key "name" % _String) yaml
--                   else Nothing
--           ?app = fromMaybe "unknown-app" $ preview (#value % key "metadata" % key "labels" % key "app" % _String) yaml
--        in objectWrite written $ view #value yaml
--     HelmValues r -> do
--       let ?namespace = view #namespace r
--       let values = cs $ Yaml.encode $ view #value yaml
--       aesons <-
--         readProcess
--           "helm"
--           [ "template"
--           , "--values"
--           , "-"
--           , view #chart r
--           ]
--           values
--           >>= Yaml.decodeAllThrow @IO @Aeson.Value . fromString
--       let objects =
--             execState
--               ( do
--                   modifying (key "metadata" % _Object) $
--                     KeyMap.insert "namespace" $
--                       Aeson.String ?namespace
--                   yaml <- get
--                   modifying (key "metadata" % key "labels" % _Object) $
--                     KeyMap.insert "app" $
--                       fromMaybe "unknown-app" $
--                         preview
--                           (key "metadata" % key "labels" % key (view (#appLabel % _Key) r))
--                           yaml
--               )
--               <$> aesons
--       let ?app =
--             fromMaybe "unknown-app" $
--               preview (_head % key "metadata" % key "labels" % key "app" % _String) objects
--       valuesPath <- path "values/"
--       putStrLn $ "# writing to: " <> valuesPath
--       ByteString.writeFile valuesPath $ Yaml.encode $ view #value yaml
--       written <- processYaml written $ Util.manifest Util.namespace
--       foldlM objectWrite written objects
--  where
--   path dir = do
--     createDirectoryIfMissing True dir
--     pure $ dir <> Text.unpack ?app <> ".yaml"
--   objectWrite :: (?namespace :: Text, ?app :: Text) => [FilePath] -> Aeson.Value -> IO [FilePath]
--   objectWrite written object = do
--     path <-
--       path $
--         "manifest/"
--           <> Text.unpack ?namespace
--           <> "/"
--           <> ( let neverDelete = "never-delete/"
--                 in case preview (key "kind") object of
--                     Just "PersistentVolumeClaim" -> neverDelete
--                     Just "Namespace" -> neverDelete
--                     _ -> mempty
--              )
--     putStrLn $ "# writing to: " <> path
--     ( if path `elem` written
--         then ByteString.appendFile path . ("---\n" <>)
--         else ByteString.writeFile path
--       )
--       $ Yaml.encode object
--     pure $ path : written

-- generate :: IO ()
-- generate = void $ foldlM processYaml [] yamls

-- generate :: IO ()
-- generate = void $ foldlM processYaml [] yamls

generate :: IO ()
generate = traverse_ processProject projects

projectDir :: FilePath
projectDir = "project"

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
