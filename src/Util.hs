module Util where

-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map

import Control.Monad.State.Strict (MonadState, State, execState, modify)
import Data.Aeson (FromJSON, ToJSON (toJSON), fromJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Optics (AsValue, key)
import Data.Record.Anon
import qualified Data.Record.Anon.Advanced as A (Record)
import Data.Record.Anon.Simple (Record, insert, merge, project)
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

import qualified Data.Aeson.KeyMap as KeyMap
import Secret (host)
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

domain :: (?namespace :: Text) => Text
domain = ?namespace <> "." <> host

named :: (?name :: Text) => Record _
named = ANON{name = ?name}

type ObjectMeta =
  [ "name" := Text
  , "namespace" := Text
  , "labels" := Record '["app" := Text]
  ]

meta :: (?namespace :: Text, ?name :: Text) => Record ObjectMeta
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

object :: (?namespace :: Text, ?name :: Text) => Text -> Record Object
object kind =
  ANON
    { apiVersion = v1
    , kind = kind
    , metadata = meta
    }

annotate ::
  RowHasField "metadata" r (Record ObjectMeta) =>
  KeyMap Text ->
  Record r ->
  Record _
annotate annotations object =
  merge
    object
    ANON{metadata = Anon.get #metadata object `merge` ANON{annotations = annotations}}

configMap ::
  (?namespace :: Text, ?name :: Text) =>
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

noNamespace :: Text
noNamespace = "_root"

persistentVolumeClaim :: (?namespace :: Text, ?name :: Text) => ToJSON spec => spec -> Record _
persistentVolumeClaim = setSpecTo (object "PersistentVolumeClaim")

readWriteOnce :: Text
readWriteOnce = "ReadWriteOnce"

openebsLvmClaim :: (?namespace :: Text, ?name :: Text) => Text -> Record _
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

service :: (?namespace :: Text, ?name :: Text) => Record _ -> Record _
service spec =
  setSpecTo
    (object "Service")
    $ merge ANON{selector = ANON{app = ?name}} spec

servicePort :: (?name :: Text) => Int -> Record _
servicePort port =
  ANON{name = ?name, port = port, targetPort = ?name}

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

deployment :: (?namespace :: Text, ?name :: Text) => ToJSON spec => spec -> Record _
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
  (?namespace :: Text, ?name :: Text) =>
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
    (Anon.set #apiVersion "networking.k8s.io/v1" $ object "Ingress")
    ANON
      { ingressClassName = "nginx" :: Text
      , rules = rules
      }

data PathType
  = Exact
  | Prefix
  | ImplementationSpecific
  deriving (Show)

clusterIssuer :: Text
clusterIssuer = "selfsigned-cluster-issuer"

ingressContourTls ::
  (?namespace :: Text, ?name :: Text) =>
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
ingressContourTls rules =
  annotate (KeyMap.singleton "cert-manager.io/cluster-issuer" clusterIssuer) $
    setSpecTo
      (Anon.set #apiVersion "networking.k8s.io/v1" $ object "Ingress")
      ANON
        { ingressClassName = "contour" :: Text
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
  = Manifest
  | HelmValues (Record '["chart" := String {- name of chart, like "harbor/harbor"-}])

type Yaml =
  Record
    [ "yamlType" := YamlType
    , "namespace" := Text
    , "name" := Text
    , "value" := Aeson.Value
    ]

mkYaml :: (?namespace :: Text, ?name :: Text) => Yaml
mkYaml =
  ANON
    { yamlType = Manifest
    , namespace = ?namespace
    , name = ?name
    , value = Aeson.Null
    }

mkYamlAndModify :: (?namespace :: Text, ?name :: Text) => State Yaml () -> Yaml
mkYamlAndModify = flip execState mkYaml

manifest :: (?namespace :: Text, ?name :: Text) => ToJSON json => json -> Yaml
manifest object =
  mkYamlAndModify $ do
    modify $ Anon.set #yamlType Manifest
    modify $ Anon.set #value $ toJSON object

helmValues :: (?namespace :: Text, ?name :: Text) => ToJSON json => String -> json -> Yaml
helmValues chart values =
  mkYamlAndModify $ do
    modify $ Anon.set #yamlType $ HelmValues ANON{chart = chart}
    modify $ Anon.set #value $ toJSON values

$(deriveJSON ''PathType)