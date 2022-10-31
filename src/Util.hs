module Util where

-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map

import Control.Monad.State.Strict (MonadState, execState, modify)
import Data.Aeson (FromJSON, ToJSON (toJSON), fromJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Optics (AsValue, key)
import Data.Record.Anon
import qualified Data.Record.Anon.Advanced as A (Record)
import Data.Record.Anon.Simple (Record, inject, merge)
import Data.Text (Text)
import qualified Data.Text as Text
import Optics (
  A_Setter,
  Is,
  Optic',
  over,
  set,
  (%),
 )

-- import Data.
-- import Manifest.Io.K8s.Api.Core.V1 (Namespace)
-- import Manifest.Io.K8s.Apimachinery.Pkg.Apis.Meta.V1 (ObjectMeta (..))

mirror :: (a -> a -> b) -> a -> b
mirror f a = f a a

name :: (?name :: Text) => a -> (Text, a)
name = (,) ?name

addSuffix :: (?name :: Text) => Text -> ((?name :: Text) => a) -> a
addSuffix suffix x = let ?name = ?name <> "-" <> suffix in x

systemClusterCritical :: Text
systemClusterCritical = "system-cluster-critical"

primaryLabelKey :: Text
primaryLabelKey = "app"

v1 :: Text
v1 = "v1"

setJSON ::
  (Is k A_Setter, ToJSON a) =>
  Optic' k is Aeson.Value Aeson.Value ->
  a ->
  Aeson.Value ->
  Aeson.Value
setJSON l x = set l (toJSON x)

setSpecTo :: Record object -> spec -> Record (Merge object '["spec" := spec])
setSpecTo object spec = merge object ANON{spec = spec}

assignJSON ::
  (Is k A_Setter, ToJSON a) =>
  MonadState Aeson.Value m =>
  Optic' k is Aeson.Value Aeson.Value ->
  a ->
  m ()
assignJSON l = modify . setJSON l

named :: (?name :: Text) => Record _
named = ANON{name = ?name}

type ObjectMeta =
  [ "name" := Text
  , "namespace" := Text
  , "label" := Record '["app" := Text]
  ]

meta :: (?name :: Text, ?namespace :: Text) => Record ObjectMeta
meta =
  ANON
    { name = ?name
    , namespace = ?namespace
    , label =
        ANON
          { app = ?name
          }
    }

type Object =
  [ "apiVersion" := Text
  , "kind" := Text
  , "metadata" := Record ObjectMeta
  ]

object :: (?name :: Text, ?namespace :: Text) => Text -> Record Object
object kind =
  ANON
    { apiVersion = v1
    , kind = kind
    , metadata = meta
    }

configMap ::
  (?name :: Text, ?namespace :: Text) =>
  ToJSON spec =>
  spec ->
  Record _
configMap = setSpecTo (object "ConfigMap" `merge` ANON{immutable = True})

container :: (?name :: Text) => Text -> Text -> Record _
container suffix image =
  addSuffix suffix $
    ANON
      { name = ?name
      , image = image
      , securityContext =
          ANON
            { allowPrivilegeEscalation = False
            }
      }

namedPort :: Text -> Int -> Record _
namedPort name port =
  ANON{containerPort = port, name = name}

namespace ::
  (?name :: Text, ?namespace :: Text) =>
  ToJSON spec =>
  spec ->
  Record _
namespace = setSpecTo $ object "Namespace"

persistentVolumeClaim :: (?name :: Text, ?namespace :: Text) => ToJSON spec => spec -> Record _
persistentVolumeClaim = setSpecTo (object "PersistentVolumeClaim")

readWriteOnce :: Text
readWriteOnce = "ReadWriteOnce"

openebsLvmClaim :: (?name :: Text, ?namespace :: Text) => Text -> Record _
openebsLvmClaim size =
  persistentVolumeClaim
    ANON
      { spec =
          ANON
            { storageClassName = openebsLvmProvisioner
            , accessModes = [readWriteOnce]
            , resources =
                ANON
                  { requirementsRequests = ANON{storage = size}
                  }
            }
      }
 where
  openebsLvmProvisioner :: Text
  openebsLvmProvisioner = "openebs-lvmpv"

service :: (?name :: Text, ?namespace :: Text) => ToJSON spec => spec -> Record _
service =
  setSpecTo $
    setSpecTo
      (object "Service")
      ANON{selector = ANON{api = ?name}}

configMapVolume :: (?name :: Text) => Record _
configMapVolume = merge named ANON{configMap = named}

hostPathVolume :: (?name :: Text) => Text -> Record _
hostPathVolume path = merge named ANON{hostPath = ANON{path = path}}

persistentVolumeClaimVolume :: (?name :: Text) => Record _
persistentVolumeClaimVolume =
  merge
    named
    ANON
      { persistentVolumeClaim =
          ANON
            { claimName = ?name
            , readOnly = False
            }
      }

deployment :: (?name :: Text, ?namespace :: Text) => ToJSON spec => spec -> Record _
deployment =
  setSpecTo $
    setSpecTo
      (object "Deployment")
      ANON
        { apiVersion = "apps/v1" :: Text
        , spec =
            ANON
              { replicas = 1 :: Int
              , selector = ANON{matchLabels = ANON{app = ?name}}
              , template =
                  ANON
                    { metadata = meta
                    }
              }
        }

ingress ::
  (?name :: Text, ?namespace :: Text) =>
  Record
    [ "host" := Text
    , "paths" := [Aeson.Value]
    ] ->
  Record _
ingress rules =
  setSpecTo
    (inject ANON{apiVersion = "networking.k8s.io/v1" :: Text} $ object "Ingress")
    ANON
      { ingressClassName = "nginx" :: Text
      , rules = rules
      }

-- data App = App
--   { yamlsPath :: FilePath,
--     yamlsManifests :: ![Yaml.Value]
--   }
--   deriving (Show, TH.Lift)

-- mkApp :: (?name :: Text) => App
-- mkApp =
--   App
--     { yamlsPath = "./manifests/" <> Text.unpack ?name <> ".yaml",
--       yamlsManifests = []
--     }
