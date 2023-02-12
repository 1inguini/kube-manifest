module TH (
  embedFromYamlAllFile,
  embedFromYamlFile,
  embedYamlFile,
  embedYamlAllFile,
  embedModifedYamlFile,
  deriveJSON,
  embedModifedYamlAllFile,
  yamlQQ,
) where

import Control.Monad (foldM)
import Data.Aeson (Options (sumEncoding), ToJSON (toJSON))
import Data.Aeson.Optics (members, _String)
import qualified Data.Aeson.TH as Aeson
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Record.Anon
import qualified Data.Record.Anon.Advanced as Record.Advanced
import Data.Record.Anon.Simple (Record, toAdvanced)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Yaml (decodeThrow)
import qualified Data.Yaml as Yaml
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType))
import Language.Haskell.TH.Syntax (Lift (lift), qAddDependentFile)
import Optics (ifoldrOf, over, preview)
import Util ((<:>))

notDefined :: String -> QuasiQuoter
notDefined name =
  QuasiQuoter
    { quoteExp = notDefinedField "quoteExp"
    , quotePat = notDefinedField "quotePat"
    , quoteType = notDefinedField "quoteType"
    , quoteDec = notDefinedField "quoteDec"
    }
 where
  notDefinedField :: String -> String -> TH.Q a
  notDefinedField field _ = fail (field ++ " is not defined for" <:> name)

yamlQQ :: QuasiQuoter
yamlQQ = (notDefined "yamlQQ"){quoteExp = yamlExp}

yamlExp :: String -> TH.Q TH.Exp
yamlExp str = do
  val <- TH.runIO $ decodeThrow @_ @Yaml.Value $ cs str
  case ifoldrOf members (const toRows) [] val of
    [] -> [|val|]
    fields -> do
      row <-
        foldl
          ( \acc field ->
              [t|
                (($(TH.litT . TH.strTyLit . cs $ field) ':= $(TH.varT =<< TH.newName "a")) ': $acc)
                |]
          )
          TH.promotedNilT
          fields
      let r = pure row
      vars <- TH.newName "vars"
      TH.LamE [TH.VarP vars]
        <$> [|
          let replace ::
                (AllFields $r ToJSON, KnownFields $r) => Record $r -> Yaml.Value -> Yaml.Value
              replace vars val =
                fromMaybe (over members (replace vars) val) $ do
                  text <- preview _String val
                  field <- Text.stripPrefix prefix text
                  lookup (cs field) $
                    Record.Advanced.toList $
                      Record.Advanced.cmap (Proxy :: Proxy ToJSON) (K . toJSON . unI) $
                        toAdvanced vars
           in replace (project $(TH.varE vars)) val
          |]
 where
  prefix = "$"
  toRows :: Yaml.Value -> [Text] -> [Text]
  toRows val acc = fromMaybe (ifoldrOf members (const toRows) acc val) $ do
    text <- preview _String val
    field <- Text.stripPrefix prefix text
    pure . List.nub $ field : acc

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