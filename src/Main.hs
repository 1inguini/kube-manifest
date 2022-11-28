module Main (
  main,
) where

import Manifest (yamls)
import Util (Yaml, YamlType (..))
import qualified Util

-- import Control.Applicative ((<|>))
-- import Control.Monad (void)
-- import Control.Monad.State.Strict (execState, get)
-- import qualified Data.Aeson.KeyMap as KeyMap
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString as ByteString
-- import Data.Foldable (foldlM)
-- import Data.Maybe (catMaybes, fromMaybe)
-- import Data.String (IsString (fromString))
-- import qualified Data.Yaml as Yaml (decodeAllThrow, encode)
-- import System.Directory (createDirectoryIfMissing)
-- import System.Process (readProcess)

import Control.Exception.Safe (throwString)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import Data.Aeson.Optics (AsValue (_Object, _String), key, _Key)
import Data.Either (partitionEithers)
import Data.Hashable (hash)
import Data.Record.Anon
import Data.Record.Anon.Simple (Record)
import qualified Data.Record.Anon.Simple as Anon
import Data.String.Conversions (ConvertibleStrings (convertString), convertString)
import Data.Text (Text)
import qualified Data.Text as Text
import Development.Shake (Lint (LintBasic), getShakeOptionsRules, progressSimple)
import Development.Shake.Plus
import GHC.Stack (HasCallStack)
import Optics (modifying, over, preview, set, view, (%), _head)
import Path.IO (createDir, createDirIfMissing, doesPathExist)
import System.Environment (getArgs, getExecutablePath)
import System.Linux.Namespaces (Namespace (Mount, Network, User), UserMapping (UserMapping), unshare, writeUserMappings)
import System.Posix (getEffectiveUserID)

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
--       let values = convertString $ Yaml.encode $ view #value yaml
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

-- main :: IO ()
-- main = generate

instance CmdResult r => CmdArguments (RAction e r) where
  cmdArguments (CmdArgument x) = case partitionEithers x of
    (opts, x : xs) -> liftAction $ command opts x xs
    _ -> error "Error, no executable or arguments given to Development.Shake.cmd"

s :: String -> String
s = id

newtype ListDynamicDep = ListDynamicDep () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ListDynamicDep = [Path Abs File]

withUnsharedRoot :: IO a -> IO a
withUnsharedRoot act = do
  uid <- getEffectiveUserID
  unshare [User]
  writeUserMappings Nothing [UserMapping 0 uid 1]
  x <- act
  writeUserMappings Nothing [UserMapping uid 0 1]
  pure x

mountFile :: Path b File -> Path b' File -> RAction _ ()
mountFile from to = do
  exist <- doesPathExist to
  writeFile' to mempty
  unless exist $ do
    cmd_ (s "mount --bind") (toFilePath from) (toFilePath to)

main :: IO ()
main = do
  uid <- getEffectiveUserID
  exe <- getExecutablePath
  args <- getArgs
  if uid /= 0
    then cmd_ (s "unshare --fork --map-root-user --mount") exe args
    else shakeArgs
      shakeOptions
        { shakeThreads = 0
        , shakeLint = Just LintBasic
        , shakeColor = True
        , shakeProgress = progressSimple
        , shakeShare = Just ".shake"
        }
      $ runShakePlus ()
      $ do
        exePath <- parseAbsFile =<< liftIO getExecutablePath
        shakeOptions <- liftRules getShakeOptionsRules
        shakeDir <- parseRelDir $ shakeFiles shakeOptions
        let buildDir = shakeDir </> [reldir|build|]
        let artifactDir = shakeDir </> [reldir|artifact|]
        let image = buildDir </> [reldir|image|]

        phony "image/scratch" $ do
          let ruleDir = buildDir </> [reldir|image/scratch|]
          sandboxed <- doesPathExist $ ruleDir </> [relfile|rootfs/run|]
          if not sandboxed
            then do
              createDirIfMissing True $ ruleDir </> [reldir|rootfs|]
              ociSpec <- readFile' [relfile|src/config.json|]
              ociSpec <- either throwString pure $ Aeson.eitherDecode (convertString ociSpec) :: _ Aeson.Value
              writeFile' (ruleDir </> [relfile|config.json|]) . convertString . Aeson.encode $
                set
                  (key "process" % key "args")
                  ( Aeson.toJSON $
                      "/run"
                        : filter
                          ( \case
                              '-' : _ -> True
                              arg -> arg == "image/scratch"
                          )
                          args
                  )
                  ociSpec
              mountFile exePath (ruleDir </> [relfile|rootfs/run|])
              let bindInRootfs :: Path Abs File -> RAction _ ()
                  bindInRootfs path = do
                    to <- replaceProperPrefix [absdir|/|] (ruleDir </> [reldir|rootfs|]) path
                    mountFile path to
              bindInRootfs [absfile|/lib64/ld-linux-x86-64.so.2|]
              bindInRootfs [absfile|/usr/lib/libc.so.6|]
              bindInRootfs [absfile|/usr/lib/libgmp.so.10|]
              bindInRootfs [absfile|/usr/lib/libm.so.6|]
              bindInRootfs [absfile|/usr/lib/libncursesw.so.6|]
              cmd_
                (Cwd $ toFilePath ruleDir)
                -- (s "unshare --fork")
                -- ["--wd=" <> toFilePath ruleDir, "--map-user=" <> show 1000]
                (s "runsc --rootless --network=none run")
                (show $ hash $ s "image/scratch")
            else do
              -- needIn
              --   (build </> [reldir|image/scratch|])
              --   [ [relfile|bin/tmp|]
              --   , [relfile|home/nonroot|]
              --   , [relfile|bin/etc/passwd|]
              --   , [relfile|etc/passwd|]
              --   , [relfile|etc/group|]
              --   , [relfile|etc/nsswitch.conf|]
              --   , [relfile|etc/ssl|]
              --   ]
              -- "container/"
              liftIO $ putStrLn "ok"
              pure ()