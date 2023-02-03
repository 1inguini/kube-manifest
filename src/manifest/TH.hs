module TH (
  embedFromYamlAllFile,
  embedFromYamlFile,
  embedYamlFile,
  embedYamlAllFile,
  embedModifedYamlFile,
  deriveJSON,
  embedModifedYamlAllFile,
  yamlExp,
  objectQQ,
) where

import Control.Monad (foldM)
import Data.Aeson (Options (sumEncoding), ToJSON (toJSON))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (members, _String)
import qualified Data.Aeson.TH as Aeson
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Record.Anon
import qualified Data.Record.Anon.Advanced as Record.Advanced
import Data.Record.Anon.Simple (Record, toAdvanced)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Yaml (decodeThrow)
import qualified Data.Yaml as Yaml
import GHC.TypeLits (Symbol)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType))
import Language.Haskell.TH.Syntax (Lift (lift), qAddDependentFile)
import Optics (over, preview)
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

objectQQ :: QuasiQuoter
objectQQ =
  (notDefined "objectQQ")
    { quoteExp = \str -> do
        lift =<< TH.runIO (decodeThrow @_ @Yaml.Object $ cs str)
    }

-- yamlExp :: String -> TH.Q TH.Exp
-- yamlExp str = do
--   val <- TH.runIO $ decodeThrow @_ @Yaml.Value $ cs str
--   lift val

yamlExp :: [String] -> String -> TH.Q TH.Exp
yamlExp varNames str = do
  val <- TH.runIO $ decodeThrow @_ @Yaml.Value $ cs str
  row <-
    TH.parensT
      $ foldM
        ( \exp field -> do
            [t|($(TH.litT $ TH.strTyLit field) ':= $(TH.varT =<< TH.newName "a")) ': ($(pure exp))|]
        )
        TH.PromotedNilT
      $ List.reverse varNames
  let r = pure row
  vars <- TH.newName "vars"
  TH.LamE [TH.VarP vars]
    <$> [|
      let replace :: (AllFields $r ToJSON, KnownFields $r) => Record $r -> Yaml.Value -> Yaml.Value
          replace vars val =
            fromMaybe (over members (replace vars) val) $ do
              text <- preview _String val
              lookup (cs text) $
                Record.Advanced.toList $
                  Record.Advanced.cmap (Proxy :: Proxy ToJSON) (K . toJSON . unI) $
                    toAdvanced vars
       in replace $(TH.varE vars) val
      |]

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