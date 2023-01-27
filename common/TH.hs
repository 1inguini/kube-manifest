module TH (
  embedFromYamlAllFile,
  embedFromYamlFile,
  embedYamlFile,
  embedYamlAllFile,
  embedModifedYamlFile,
  deriveJSON,
  embedModifedYamlAllFile,
) where

import Data.Aeson (Options (sumEncoding))
import qualified Data.Aeson.TH as Aeson
import qualified Data.Yaml as Yaml
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (Lift (lift), qAddDependentFile)

getYamlFile :: forall a. Yaml.FromJSON a => FilePath -> TH.Q a
getYamlFile path =
  qAddDependentFile path
    >> TH.runIO (Yaml.decodeFileThrow path :: IO a)

getYamlAllFile :: forall a. Yaml.FromJSON a => FilePath -> TH.Q [a]
getYamlAllFile path =
  qAddDependentFile path
    >> TH.runIO (Yaml.decodeAllFileThrow path :: IO [a])

embedFrom ::
  forall a b.
  Lift b =>
  (FilePath -> TH.Q a) ->
  FilePath ->
  (a -> TH.Q b) ->
  TH.Q TH.Exp
embedFrom get path conv = get path >>= conv >>= lift

embedFromYamlFile ::
  forall a b.
  (Yaml.FromJSON a, Lift b) =>
  FilePath ->
  (a -> TH.Q b) ->
  TH.Q TH.Exp
embedFromYamlFile = embedFrom getYamlFile

embedFromYamlAllFile ::
  forall a b.
  (Yaml.FromJSON a, Lift b) =>
  FilePath ->
  ([a] -> TH.Q b) ->
  TH.Q TH.Exp
embedFromYamlAllFile = embedFrom getYamlAllFile

embedYamlFile :: FilePath -> TH.Q TH.Exp
embedYamlFile path = embedFromYamlFile @Yaml.Value path pure

embedYamlAllFile :: FilePath -> TH.Q TH.Exp
embedYamlAllFile path = embedFromYamlAllFile @Yaml.Value path pure

embedModifedYamlFile ::
  forall a b.
  (Yaml.FromJSON a, Yaml.ToJSON b) =>
  FilePath ->
  (a -> b) ->
  TH.Q TH.Exp
embedModifedYamlFile path f = embedFromYamlFile @a path (pure . Yaml.toJSON . f)

embedModifedYamlAllFile :: forall a b. (Yaml.FromJSON a, Yaml.ToJSON b) => FilePath -> ([a] -> b) -> TH.Q TH.Exp
embedModifedYamlAllFile path f = embedFromYamlAllFile @a path (pure . Yaml.toJSON . f)

deriveJSON :: TH.Name -> TH.Q [TH.Dec]
deriveJSON = Aeson.deriveJSON Aeson.defaultOptions{sumEncoding = Aeson.UntaggedValue}