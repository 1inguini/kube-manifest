module Util where

-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map

import Control.Monad.State.Strict (MonadState, State, execState, modify)
import Data.Aeson (FromJSON, ToJSON (toJSON), fromJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Optics (AsValue, key)
import Data.Record.Anon
import qualified Data.Record.Anon.Advanced as A (Record)
import Data.Record.Anon.Simple (Record, inject, insert, merge, project)
import qualified Data.Record.Anon.Simple as Anon
import Data.Text (Text)
import qualified Data.Text as Text
import Optics (
  A_Setter,
  Is,
  LabelOptic',
  NoIx,
  Optic',
  over,
  set,
  view,
  (%),
 )

import TH (deriveJSON)

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
  , "labels" := Record '["app" := Text]
  ]

meta :: (?name :: Text, ?namespace :: Text) => Record ObjectMeta
meta =
  ANON
    { name = ?name
    , namespace = ?namespace
    , labels =
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
  ToJSON d =>
  d ->
  Record _
configMap d = insert #data d $ object "ConfigMap" `merge` ANON{immutable = True}

container :: (?name :: Text) => Text -> Text -> Record _ -> Record _
container suffix image rest =
  addSuffix suffix $
    ANON
      { name = ?name
      , image = image
      , securityContext =
          ANON
            { allowPrivilegeEscalation = False
            }
      }
      `merge` rest

containerPort :: (?name :: Text) => Int -> Record _
containerPort port =
  ANON{containerPort = port, name = ?name}

httpGet :: Int -> Text -> Record ["port" := Int, "path" := Text]
httpGet port path = ANON{port = port, path = path}

namespace ::
  (?namespace :: Text) =>
  Record _
namespace =
  let ?name = ?namespace
   in object "Namespace"
        `merge` ANON
          { metadata = project meta :: Record ["name" := Text, "labels" := Record '["app" := Text]]
          }

persistentVolumeClaim :: (?name :: Text, ?namespace :: Text) => ToJSON spec => spec -> Record _
persistentVolumeClaim = setSpecTo (object "PersistentVolumeClaim")

readWriteOnce :: Text
readWriteOnce = "ReadWriteOnce"

openebsLvmClaim :: (?name :: Text, ?namespace :: Text) => Text -> Record _
openebsLvmClaim size =
  persistentVolumeClaim
    ANON
      { storageClassName = openebsLvmProvisioner
      , accessModes = [readWriteOnce]
      , resources =
          ANON
            { requests = ANON{storage = size}
            }
      }
 where
  openebsLvmProvisioner :: Text
  openebsLvmProvisioner = "openebs-lvmpv"

service :: (?name :: Text, ?namespace :: Text) => Record _ -> Record _
service spec =
  setSpecTo
    (object "Service")
    $ merge ANON{selector = ANON{app = ?name}} spec

servicePort :: (?name :: Text) => Int -> Record _
servicePort port =
  ANON{port = port, name = ?name}

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

volumeMount :: (?name :: Text) => Text -> Record _
volumeMount mountPath = ANON{name = ?name, mountPath = mountPath}

deployment :: (?name :: Text, ?namespace :: Text) => ToJSON spec => spec -> Record _
deployment spec =
  object "Deployment"
    `merge` ANON
      { apiVersion = "apps/v1" :: Text
      , spec =
          ANON
            { replicas = 1 :: Int
            , selector = ANON{matchLabels = ANON{app = ?name}}
            , template =
                ANON
                  { metadata = meta
                  , spec = spec
                  }
            }
      }

ingressNginx ::
  (?name :: Text, ?namespace :: Text) =>
  (AllFields backend ToJSON) =>
  [ Record
      [ "host" := Text
      , "http"
          := Record
              '[ "paths"
                  := [ Record
                        [ "backend" := Record backend
                        , "path" := Text
                        , "pathType" := PathType
                        ]
                     ]
               ]
      ]
  ] ->
  Record _
ingressNginx rules =
  setSpecTo
    (inject ANON{apiVersion = "networking.k8s.io/v1" :: Text} $ object "Ingress")
    ANON
      { ingressClassName = "nginx" :: Text
      , rules = rules
      }

data PathType
  = Exact
  | Prefix
  | ImplementationSpecific
  deriving (Show)

ingressNginxTls ::
  (?name :: Text, ?namespace :: Text) =>
  (AllFields back ToJSON) =>
  [ Record
      [ "host" := Text
      , "http"
          := Record
              '[ "paths"
                  := [ Record
                        [ "backend" := Record back
                        , "path" := Text
                        , "pathType" := PathType
                        ]
                     ]
               ]
      ]
  ] ->
  Record _
ingressNginxTls rules =
  setSpecTo
    (inject ANON{apiVersion = "networking.k8s.io/v1" :: Text} $ object "Ingress")
    ANON
      { ingressClassName = "nginx" :: Text
      , rules = rules
      , tls = [ANON{hosts = view #host <$> rules}]
      }

ingressRule :: (?name :: Text) => Text -> Record _
ingressRule host =
  ANON
    { host = host
    , http =
        ANON
          { paths =
              [ ANON
                  { backend = backend
                  , path = "/" :: Text
                  , pathType = Prefix
                  }
              ]
          }
    }

backend :: (?name :: Text) => Record _
backend = ANON{service = ANON{name = ?name, port = named}}

type Manifest = Record ["path" := FilePath, "objects" := [Aeson.Value]]

data YamlType
  = ManifestNoNamespace
  | Manifest (Record '["namespace" := Text])
  | HelmValues (Record ["namespace" := Text, "chart" := String {- name of chart, like "harbor/harbor"-}])

type Yaml =
  Record
    [ "yamlType" := YamlType
    , "name" := Text
    , "value" := Aeson.Value
    ]

mkYaml :: (?name :: Text) => Yaml
mkYaml =
  ANON
    { yamlType = ManifestNoNamespace
    , name = ?name
    , value = Aeson.Null
    }

mkYamlAndModify :: (?name :: Text) => State Yaml () -> Yaml
mkYamlAndModify = flip execState mkYaml

manifest :: (?namespace :: Text, ?name :: Text) => [Aeson.Value] -> [Yaml]
manifest =
  fmap
    ( \object -> mkYamlAndModify $ do
        modify $ Anon.set #yamlType $ Manifest ANON{namespace = ?namespace}
        modify $ Anon.set #value object
    )

manifestNoNamespace :: (?name :: Text) => [Aeson.Value] -> [Yaml]
manifestNoNamespace =
  fmap (\object -> Anon.set #value object mkYaml)

helmValues :: (?name :: Text, ?namespace :: Text) => String -> Aeson.Value -> Yaml
helmValues chart values =
  mkYamlAndModify $ do
    modify $ Anon.set #yamlType $ HelmValues ANON{namespace = ?namespace, chart = chart}
    modify $ Anon.set #value values

$(deriveJSON ''PathType)