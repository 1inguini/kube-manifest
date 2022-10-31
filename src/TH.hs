module TH (
  embedFromYamlFile,
  embedYamlFile,
  embedModifedYamlFile,
  deriveJSON,
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

embedFromYamlFile ::
  forall a b.
  (Yaml.FromJSON a, Lift b) =>
  FilePath ->
  (a -> TH.Q b) ->
  TH.Q TH.Exp
embedFromYamlFile path conv = getYamlFile path >>= conv >>= lift

embedYamlFile :: FilePath -> TH.Q TH.Exp
embedYamlFile path = embedFromYamlFile @Yaml.Value path pure

embedModifedYamlFile :: forall a b. (Yaml.FromJSON a, Yaml.ToJSON b) => FilePath -> (a -> b) -> TH.Q TH.Exp
embedModifedYamlFile path f = embedFromYamlFile @a path (pure . Yaml.toJSON . f)

deriveJSON :: TH.Name -> TH.Q [TH.Dec]
deriveJSON = Aeson.deriveJSON Aeson.defaultOptions{sumEncoding = Aeson.UntaggedValue}