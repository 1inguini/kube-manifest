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

import Data.Aeson (Key, Options (sumEncoding), ToJSON (toJSON))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.TH as Aeson
import qualified Data.List as List
import Data.Maybe (fromMaybe, maybeToList)
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Yaml (decodeThrow)
import qualified Data.Yaml as Yaml
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType))
import Language.Haskell.TH.Syntax (Lift (lift), qAddDependentFile)
import Optics (ifoldMapOf, ifolded, ifoldlOf')

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
here =
  (notDefined "here")
    { quoteExp = \s -> lift $ case s of
        '\n' : s -> s
        s -> s
    }

objQQ :: QuasiQuoter
objQQ = (notDefined "objQQ"){quoteExp = objExp}

objExp :: String -> TH.Q TH.Exp
objExp str = do
  obj <- TH.runIO $ decodeThrow @_ @Yaml.Object $ cs str
  case (\(x, y, z) -> (List.nub x, List.nub y, List.nub z)) $ objectSearch obj of
    ([], [], []) -> [|obj|]
    (vals, fields, macros) ->
      let varOrImp ('?' : x) = TH.implicitParamVarE x
          varOrImp x = TH.varE $ TH.mkName x
          valDictE = TH.listE $ (\x -> [|(x, toJSON $(varOrImp x))|]) <$> vals
          fieldDictE = TH.listE $ (\x -> [|(x, $(varOrImp x))|]) <$> fields
          macroDictE = TH.listE $ (\x -> [|(x, $(varOrImp x) . toJSON)|]) <$> macros
       in [|
            let valDict :: [(String, Yaml.Value)]
                valDict = $valDictE
                fieldDict :: [(String, Yaml.Object)]
                fieldDict = $fieldDictE
                macroDict :: [(String, Yaml.Value -> Yaml.Object)]
                macroDict = $macroDictE
                objectReplace :: Yaml.Object -> Yaml.Object
                objectReplace = ifoldlOf' ifolded (\k acc -> mergeObject acc . kvReplace k) mempty
                kvReplace :: Key -> Yaml.Value -> Yaml.Object
                kvReplace key Yaml.Null =
                  fromMaybe (KeyMap.singleton key Yaml.Null) $ do
                    var <- Text.stripPrefix prefix $ Key.toText key
                    lookup (cs var) fieldDict
                kvReplace key val =
                  let replaced = valueReplace val
                   in fromMaybe (KeyMap.singleton key replaced) $ do
                        var <- Text.stripPrefix prefix $ Key.toText key
                        macro <- lookup (cs var) macroDict
                        pure $ macro replaced
                valueReplace :: Yaml.Value -> Yaml.Value
                valueReplace (Yaml.Object obj) = Yaml.Object $ objectReplace obj
                valueReplace (Yaml.Array vec) = Yaml.Array $ valueReplace <$> vec
                valueReplace str@(Yaml.String text) = fromMaybe str $ do
                  var <- Text.stripPrefix prefix text
                  lookup (cs var) valDict
                valueReplace val = val
             in objectReplace obj
            |]
 where
  prefix = "$"
  objectSearch ::
    Yaml.Object ->
    ( [String] -- `a: $b`
    , [String] -- `$a: null`
    , [String] -- `$a: { b: c }`
    )
  objectSearch = ifoldMapOf ifolded keySearch
  keySearch :: Key -> Yaml.Value -> ([String], [String], [String])
  keySearch key Yaml.Null = (mempty, maybeToList . List.stripPrefix prefix $ Key.toString key, mempty)
  keySearch key val = valueSearch val <> (mempty, mempty, maybeToList . List.stripPrefix prefix $ Key.toString key)
  valueSearch :: Yaml.Value -> ([String], [String], [String])
  valueSearch (Yaml.Object obj) = objectSearch obj
  valueSearch (Yaml.Array vec) = mconcat $ valueSearch <$> Vector.toList vec
  valueSearch (Yaml.String text) = (maybeToList $ List.stripPrefix prefix $ cs text, mempty, mempty)
  valueSearch _ = mempty

mergeYaml :: Yaml.Value -> Yaml.Value -> Yaml.Value
mergeYaml (Yaml.Array x) (Yaml.Array y) = Yaml.Array $ x <> y
mergeYaml (Yaml.Object x) (Yaml.Object y) = Yaml.Object $ mergeObject x y
mergeYaml x _ = x

mergeObject :: Yaml.Object -> Yaml.Object -> Yaml.Object
mergeObject = KeyMap.unionWith mergeYaml

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

embedYamlFile :: forall a. (Yaml.FromJSON a, Lift a) => FilePath -> TH.Q TH.Exp
embedYamlFile path = embedFromYamlFile @a path pure

embedYamlAllFile :: forall a. (Yaml.FromJSON a, Lift a) => FilePath -> TH.Q TH.Exp
embedYamlAllFile path = embedFromYamlAllFile @a path pure

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