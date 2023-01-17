module Util (
  Manifest,
  Yaml,
  YamlType (..),
  annotate,
  assignJSON,
  clusterIssuer,
  configMap,
  configMapVolume,
  container,
  containerPort,
  deployment,
  domain,
  emptyDirVolume,
  execCommandProbe,
  helmValues,
  hostPathVolume,
  httpGetProbe,
  httpServicePort,
  ingressContourTls,
  ingressContourTlsAnnotations,
  ingressRule,
  labelSelector,
  manifest,
  meta,
  mirror,
  mkYaml,
  name,
  named,
  namespace,
  noNamespace,
  nonroot,
  object,
  openebsLvmClaim,
  persistentVolumeClaim,
  persistentVolumeClaimVolume,
  readWriteOnce,
  registry,
  s,
  service,
  servicePort,
  setJSON,
  setSpecTo,
  statefulSet,
  systemClusterCritical,
  tcpSocketProbe,
  v1,
  volumeMount,
  workload,
) where

import Control.Monad.State.Strict (MonadState, modify)
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (AsValue (_Object), key)
import Data.Record.Anon
import Data.Record.Anon.Simple (Record, insert, merge, project)
import qualified Data.Record.Anon.Simple as Anon
import Data.Text (Text)
import Optics (A_Setter, Is, Optic', over, set, view, (%))

import Secret (host)
import TH (deriveJSON)

-- import Data.
-- import Manifest.Io.K8s.Api.Core.V1 (Namespace)
-- import Manifest.Io.K8s.Apimachinery.Pkg.Apis.Meta.V1 (ObjectMeta (..))

s :: String -> String
s = id

registry :: Text
registry = "registry." <> host <> "/library/"

mirror :: (a -> a -> b) -> a -> b
mirror f a = f a a

name :: Text -> ((?name :: Text) => a) -> a
name name x = let ?name = name in x

-- addSuffix :: (?name :: Text) => Text -> ((?name :: Text) => a) -> a
-- addSuffix suffix x = let ?name = ?name <> "-" <> suffix in x

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

labelSelector :: (?app :: Text) => Record _
labelSelector = ANON{selector = ANON{app = ?app}}

type ObjectMeta =
  [ "name" := Text
  , "namespace" := Text
  , "labels" := Record '["app" := Text]
  ]

meta :: (?namespace :: Text, ?app :: Text, ?name :: Text) => Record ObjectMeta
meta =
  ANON
    { name = ?name
    , namespace = ?namespace
    , labels =
        ANON
          { app = ?app
          }
    }

type Object =
  [ "apiVersion" := Text
  , "kind" := Text
  , "metadata" := Record ObjectMeta
  ]

object :: (?namespace :: Text, ?app :: Text, ?name :: Text) => Text -> Record Object
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
  (?namespace :: Text, ?app :: Text, ?name :: Text) =>
  ToJSON d =>
  d ->
  Record _
configMap d = insert #data d $ object "ConfigMap" `merge` ANON{immutable = True}

container :: (?name :: Text) => Text -> Record _ -> Record _
container image rest =
  ANON
    { name = ?name
    , image = image
    , securityContext =
        ANON
          { allowPrivilegeEscalation = False
          }
    , imagePullPolicy = "Always" :: Text
    }
    `merge` rest

containerPort :: (?name :: Text) => Int -> Record _
containerPort port =
  ANON{containerPort = port, name = ?name}

httpGetProbe :: (?name :: Text) => Text -> Record _
httpGetProbe path = ANON{httpGet = ANON{port = ?name, path = path}}

tcpSocketProbe :: (?name :: Text) => Record _
tcpSocketProbe = ANON{tcpSocket = ANON{port = ?name}}

execCommandProbe :: [Text] -> Record _
execCommandProbe command = ANON{exec = ANON{command = command}}

namespace ::
  (?namespace :: Text, ?app :: Text) =>
  Record _
namespace =
  let ?name = ?namespace
   in object "Namespace"
        `merge` ANON
          { metadata = project meta :: Record ["name" := _, "labels" := _]
          }

noNamespace :: Text
noNamespace = "_root"

persistentVolumeClaim :: (?namespace :: Text, ?app :: Text, ?name :: Text) => ToJSON spec => spec -> Record _
persistentVolumeClaim = setSpecTo (object "PersistentVolumeClaim")

readWriteOnce :: Text
readWriteOnce = "ReadWriteOnce"

openebsLvmClaim :: (?namespace :: Text, ?app :: Text, ?name :: Text) => Text -> Record _
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

service :: (?namespace :: Text, ?app :: Text, ?name :: Text) => Record _ -> Record _
service spec =
  setSpecTo
    (object "Service")
    $ merge labelSelector spec

servicePort :: (?name :: Text) => Int -> Record _
servicePort port =
  ANON{name = ?name, port = port, targetPort = ?name}

httpServicePort :: (?name :: Text) => Record _
httpServicePort =
  ANON{name = ?name, port = 80 :: Int, targetPort = ?name}

configMapVolume :: (?name :: Text) => Record _
configMapVolume = merge named ANON{configMap = named}

hostPathVolume :: (?name :: Text) => Text -> Record _
hostPathVolume path = merge named ANON{hostPath = ANON{path = path}}

emptyDirVolume :: (?name :: Text) => Record _
emptyDirVolume = merge named ANON{emptyDir = KeyMap.empty :: KeyMap ()}

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

nonroot :: Int
nonroot = 65532

workload ::
  (?namespace :: Text, ?app :: Text, ?name :: Text) =>
  (ToJSON (Record spec), ToJSON podTemplateSpec) =>
  Text ->
  Record spec ->
  podTemplateSpec ->
  Record _
workload kind spec podTemplateSpec =
  object kind
    `merge` ANON
      { apiVersion = "apps/v1" :: Text
      , spec =
          ANON
            { replicas = 1 :: Int
            , selector = ANON{matchLabels = ANON{app = ?name}}
            , template =
                ANON
                  { metadata = meta
                  , spec = podTemplateSpec
                  }
            }
            `merge` spec
      }

deployment :: (?namespace :: Text, ?app :: Text, ?name :: Text) => ToJSON spec => spec -> Record _
deployment =
  workload "Deployment" ANON{strategy = Anon.insert #type ("Recreate" :: Text) ANON{}}

statefulSet ::
  (?namespace :: Text, ?app :: Text, ?name :: Text) =>
  (ToJSON podTemplateSpec, ToJSON persistentVolumeClaim) =>
  podTemplateSpec ->
  [persistentVolumeClaim] ->
  Record _
statefulSet podTemplateSpec persistentVolumeClaims =
  workload
    "StatefulSet"
    ANON
      { serviceName = ?name
      , podManagementPolicy = "Parallel" :: Text
      , volumeClaimTemplates = persistentVolumeClaims
      }
    podTemplateSpec

data PathType
  = Exact
  | Prefix
  | ImplementationSpecific
  deriving (Show)

clusterIssuer :: Text
clusterIssuer = "1inguini-ca-cluster-issuer"

ingressContourTlsAnnotations :: KeyMap Text
ingressContourTlsAnnotations =
  KeyMap.fromList
    [ ("cert-manager.io/cluster-issuer", clusterIssuer)
    , ("ingress.kubernetes.io/force-ssl-redirect", "true")
    ]

ingressContourTls ::
  (?namespace :: Text, ?app :: Text, ?name :: Text) =>
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
  annotate ingressContourTlsAnnotations $
    setSpecTo
      (Anon.set #apiVersion "networking.k8s.io/v1" $ object "Ingress")
      ANON
        { ingressClassName = "contour" :: Text
        , rules = rules
        , tls = [ANON{secretName = ?name, hosts = view #host <$> rules}]
        }

ingressRule :: (?namespace :: Text, ?name :: Text) => Text -> Record _
ingressRule path =
  ANON
    { host = domain
    , http =
        ANON
          { paths =
              [ ANON
                  { backend = backend
                  , path = path
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
  | HelmValues
      ( Record
          '[ "chart" := String -- name of chart, like "harbor/harbor"
           , "namespace" := Text
           , "appLabel" := Text
           ]
      )

type Yaml =
  Record
    [ "yamlType" := YamlType
    , "value" := Aeson.Value
    ]

mkYaml :: (?namespace :: Text, ?app :: Text) => YamlType -> ToJSON json => ((?name :: Text) => json) -> Yaml
mkYaml ty value =
  ANON
    { yamlType = ty
    , value =
        over (key "metadata" % _Object) (KeyMap.insert "namespace" $ Aeson.String ?namespace) $
          let ?name = ?app in toJSON value
    }

manifest :: (?namespace :: Text, ?app :: Text) => ToJSON json => ((?name :: Text) => json) -> Yaml
manifest = mkYaml Manifest

helmValues ::
  (?namespace :: Text, ?app :: Text) =>
  ToJSON json =>
  Record '["chart" := String, "appLabel" := Text] ->
  json ->
  Yaml
helmValues meta =
  mkYaml
    ( HelmValues
        ANON
          { chart = view #chart meta
          , namespace = ?namespace
          , appLabel = view #appLabel meta
          }
    )

$(deriveJSON ''PathType)