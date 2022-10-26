module TH where

import Data.List.Optics qualified as Optics
import Data.Maybe (fromMaybe)
import qualified Data.Yaml as Yaml
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Optics
  ( DefName (TopName),
    FieldNamer,
    LensRules,
    fieldLabelsRules,
    lensField,
    makeFieldLabelsWith,
    preview,
    set,
  )

getYamlFile :: forall a. Yaml.FromJSON a => FilePath -> TH.Q a
getYamlFile path =
  TH.qAddDependentFile path
    >> TH.runIO (Yaml.decodeFileThrow path :: IO a)

embedFromYamlFile ::
  forall a b.
  (Yaml.FromJSON a, TH.Lift b) =>
  FilePath ->
  (a -> TH.Q b) ->
  TH.Q TH.Exp
embedFromYamlFile path conv = getYamlFile path >>= conv >>= TH.lift

embedYamlFile :: FilePath -> TH.Q TH.Exp
embedYamlFile path = embedFromYamlFile @Yaml.Value path pure

embedModifedYamlFile :: forall a b. (Yaml.FromJSON a, Yaml.ToJSON b) => FilePath -> (a -> b) -> TH.Q TH.Exp
embedModifedYamlFile path f = embedFromYamlFile @a path (pure . Yaml.toJSON . f)
