module TH (
  deriveJSON,
  embedFromYamlAllFile,
  embedFromYamlFile,
  embedModifedYamlAllFile,
  embedModifedYamlFile,
  embedYamlAllFile,
  embedYamlFile,
  objQQ,
  here,
) where

import Control.Arrow (Arrow (first))
import Data.Aeson (Key, Options (sumEncoding), ToJSON (toJSON))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (members, _Object, _String)
import qualified Data.Aeson.TH as Aeson
import qualified Data.List as List
import Data.Maybe (fromMaybe, maybeToList)
import Data.Record.Anon
import qualified Data.Record.Anon.Advanced as Record.Advanced
import Data.Record.Anon.Simple (Record, project, toAdvanced)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Yaml (decodeThrow)
import qualified Data.Yaml as Yaml
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType))
import Language.Haskell.TH.Syntax (Lift (lift), qAddDependentFile)
import Optics (both, ifoldMapOf, ifolded, ifoldrOf, over, preview)

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
  notDefinedField field _ = fail (field <> " is not defined for " <> name)

here :: QuasiQuoter
here = (notDefined "here"){quoteExp = lift}

objQQ :: QuasiQuoter
objQQ = (notDefined "objQQ"){quoteExp = objExp}

objExp :: String -> TH.Q TH.Exp
objExp str = do
  obj <- TH.runIO $ decodeThrow @_ @Yaml.Object $ cs str
  case over both List.nub $ vars obj of
    ([], []) -> [|obj|]
    (vals, fields) -> do
      valTyVars <- traverse (\x -> (,) x <$> TH.newName "a") vals
      valsRow <-
        foldl
          ( \acc (val, a) ->
              [t|
                (($(TH.litT . TH.strTyLit . cs $ val) := $(TH.varT a)) : $acc)
                |]
          )
          TH.promotedNilT
          valTyVars
      fieldsRow <-
        foldl
          ( \acc field ->
              [t|
                (($(TH.litT . TH.strTyLit . cs $ field) := Yaml.Object) : $acc)
                |]
          )
          TH.promotedNilT
          fields
      let vr = pure valsRow
          fr = pure fieldsRow
      row <-
        foldl
          ( \acc field ->
              [t|
                (($(TH.litT . TH.strTyLit . cs $ field) := Yaml.Object) : $acc)
                |]
          )
          vr
          fields
      let r = pure row
          sig =
            TH.forallT
              ((\(_, a) -> TH.PlainTV a TH.SpecifiedSpec) <$> valTyVars)
              (fmap @TH.Q (: []) [t|(AllFields $vr ToJSON)|])
              [t|Record $r -> Yaml.Object -> Yaml.Object|]
      vars <- TH.newName "vars"
      TH.LamE [TH.VarP vars]
        <$> [|
          let replace :: $sig
              replace record obj =
                let valDict :: [(Text, Yaml.Value)]
                    valDict =
                      fmap (first cs)
                        . Record.Advanced.toList
                        . Record.Advanced.cmap (Proxy :: Proxy ToJSON) (K . toJSON . unI)
                        . toAdvanced
                        $ project @_ @($vr) record
                    fieldDict :: [(Text, Yaml.Object)]
                    fieldDict =
                      fmap (first cs)
                        . Record.Advanced.toList
                        . Record.Advanced.cmap (Proxy :: Proxy ((~) Yaml.Object)) (K . unI)
                        . toAdvanced
                        $ project @_ @($fr) record
                    step :: Yaml.Object -> Yaml.Object
                    step = ifoldMapOf ifolded replace
                    replace :: Key -> Yaml.Value -> Yaml.Object
                    replace key (Yaml.Object obj) =
                      KeyMap.singleton key . Yaml.Object $ step obj
                    replace key str@(Yaml.String text) =
                      KeyMap.singleton key $ fromMaybe str $ do
                        var <- Text.stripPrefix prefix text
                        lookup var valDict
                    replace key Yaml.Null =
                      fromMaybe (KeyMap.singleton key Yaml.Null) $ do
                        var <- Text.stripPrefix prefix $ Key.toText key
                        lookup var fieldDict
                    replace key val = KeyMap.singleton key val
                 in step obj
           in replace (project $(TH.varE vars)) obj
          |]
 where
  prefix = "$"
  vars :: Yaml.Object -> ([Text], [Text])
  vars = ifoldMapOf ifolded vars'
  vars' :: Key -> Yaml.Value -> ([Text], [Text])
  vars' _ (Yaml.Object obj) = vars obj
  vars' _ (Yaml.String text) = (maybeToList $ Text.stripPrefix prefix text, mempty)
  vars' key Yaml.Null = (mempty, maybeToList . Text.stripPrefix prefix $ Key.toText key)
  vars' _ _ = mempty

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