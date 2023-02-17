module Util (
  Helm,
  Manifest,
  Project,
  Yaml,
  YamlType (..),
  assignJSON,
  clusterIssuer,
  configMap,
  configMapVolume,
  container,
  containerPort,
  defaultHelm,
  defineHelm,
  deployment,
  domain,
  emptyDirVolume,
  encodeAll,
  execCommandProbe,
  helmValues,
  hostPathVolume,
  httpGetProbe,
  httpServicePort,
  ingressContourTls,
  ingressContourTlsAnnotations,
  ingressRule,
  issuer,
  labelSelector,
  manifest,
  meta,
  mirror,
  mkYaml,
  name,
  named,
  noNamespace,
  nonrootGid,
  nonrootOwn,
  nonrootUid,
  object,
  openebsLvmClaim,
  persistentVolumeClaim,
  persistentVolumeClaimVolume,
  readWriteOnce,
  registry,
  rootGid,
  rootOwn,
  rootUid,
  service,
  servicePort,
  setJSON,
  setSpecTo,
  statefulSet,
  systemClusterCritical,
  tcpSocketProbe,
  toObj,
  v1,
  volumeMount,
  werfProject,
  workload,
  openebsLvmProvisioner,
) where

import Secret (cloudflareOriginCAKey, host)
import TH (deriveJSON, objQQ)

import Control.Arrow (first)
import Control.Monad.State.Strict (MonadState, modify)
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (AsValue (_Object), key)
import Data.ByteString (ByteString)
import Data.Record.Anon (AllFields, K (K), KnownFields, Merge, Proxy (..), RowHasField, SubRow, unI, pattern (:=))
import qualified Data.Record.Anon.Advanced as Record.Advanced
import Data.Record.Anon.Simple (Record, inject, insert, merge)
import qualified Data.Record.Anon.Simple as Anon
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import Optics (A_Setter, Is, Optic', over, set, view, (%))
import System.Posix (GroupID, UserID)

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

toObj :: (AllFields r ToJSON, KnownFields r) => Record r -> Yaml.Object
toObj =
  KeyMap.fromList
    . fmap (first Key.fromString)
    . Record.Advanced.toList
    . Record.Advanced.cmap (Proxy :: Proxy ToJSON) (K . toJSON . unI)
    . Anon.toAdvanced

domain :: (?project :: Text) => Text
domain = ?project <> "." <> host

named :: (?name :: Text) => Yaml.Object
named = toObj ANON{name = ?name}

labelSelector :: (?app :: Text) => Yaml.Object
labelSelector = [objQQ| selector: { app: $app } |] ANON{app = ?app}

type Owner = (UserID, GroupID)

nonrootOwn :: Owner
nonrootOwn = (nonrootUid, nonrootGid)
nonrootUid :: UserID
nonrootUid = 65532
nonrootGid :: GroupID
nonrootGid = 65532

rootOwn :: Owner
rootOwn = (rootUid, rootGid)
rootUid :: UserID
rootUid = 0
rootGid :: GroupID
rootGid = 0

meta :: (?app :: Text, ?name :: Text) => Yaml.Object
meta =
  [objQQ|
name: $name
labels:
  app: $app
|]
    ANON
      { name = ?name
      , app = ?app
      }

object :: (?app :: Text, ?name :: Text) => Text -> Yaml.Object
object kind =
  [objQQ|
apiVersion: v1
kind: $kind
metadata: $meta
|]
    ANON{kind = kind, meta = meta}

type Object =
  [ "apiVersion" := Text
  , "kind" := Text
  , "metadata" := Yaml.Object
  ]

type ObjectMeta =
  [ "name" := Text
  , "labels" := Record '["app" := Text]
  ]

-- annotate ::
--   RowHasField "metadata" r (Record ObjectMeta) =>
--   KeyMap Text ->
--   Record r ->
--   Record _
-- annotate annotations object =
--   merge
--     object
--     ANON{metadata = Anon.get #metadata object `merge` ANON{annotations = annotations}}

configMap :: (?app :: Text, ?name :: Text) => ToJSON d => d -> Yaml.Object
configMap d = object "ConfigMap" <> [objQQ|{ immutable: true, data: $d }|] ANON{d = d}

container :: (?name :: Text) => Text -> Yaml.Object -> Yaml.Object
container image rest =
  [objQQ|
name: $name
image: $image
securityContext:
  allowPrivilegeEscalation: false
imagePullPolicy: Always
|]
    ANON
      { name = ?name
      , image = image
      }
    <> rest

containerPort :: (?name :: Text) => Int -> Yaml.Object
containerPort port = toObj ANON{containerPort = port, name = ?name}

httpGetProbe :: (?name :: Text) => Text -> Yaml.Object
httpGetProbe path = toObj ANON{httpGet = ANON{port = ?name, path = path}}

tcpSocketProbe :: (?name :: Text) => Yaml.Object
tcpSocketProbe = toObj ANON{tcpSocket = ANON{port = ?name}}

execCommandProbe :: [Text] -> Yaml.Object
execCommandProbe command = toObj ANON{exec = ANON{command = command}}

-- namespace :: (?namespace :: Text, ?app :: Text) => Record _
-- namespace =
--   let ?name = ?namespace
--    in object "Namespace"
--         `merge` ANON
--           { metadata = Anon.project meta :: Record ["name" := _, "labels" := _]
--           }

noNamespace :: Text
noNamespace = "_root"

persistentVolumeClaim :: (?app :: Text, ?name :: Text) => ToJSON spec => spec -> Yaml.Object
persistentVolumeClaim spec = object "PersistentVolumeClaim" <> toObj ANON{spec = spec}

readWriteOnce :: Text
readWriteOnce = "ReadWriteOnce"

openebsLvmClaim :: (?app :: Text, ?name :: Text) => Text -> Yaml.Object
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

openebsLvmProvisioner :: Text
openebsLvmProvisioner = "openebs-lvmpv"

service :: (?app :: Text, ?name :: Text) => Yaml.Object -> Yaml.Object
service spec =
  object "Service"
    <> toObj ANON{spec = labelSelector <> spec}

servicePort :: (?name :: Text) => Int -> Yaml.Object
servicePort port = toObj ANON{name = ?name, port = port, targetPort = ?name}

httpServicePort :: (?name :: Text) => Yaml.Object
httpServicePort = toObj ANON{name = ?name, port = 80 :: Int, targetPort = ?name}

configMapVolume :: (?name :: Text) => Yaml.Object
configMapVolume = named <> toObj ANON{configMap = named}

hostPathVolume :: (?name :: Text) => Text -> Yaml.Object
hostPathVolume path = named <> toObj ANON{hostPath = ANON{path = path}}

emptyDirVolume :: (?name :: Text) => Yaml.Object
emptyDirVolume = named <> [objQQ|emptyDir: {}|]

persistentVolumeClaimVolume :: (?name :: Text) => Yaml.Object
persistentVolumeClaimVolume =
  named
    <> [objQQ|
persistentVolumeClaim:
  claimName: $name
  readOnly: false
|]
      ANON{name = ?name}

volumeMount :: (?name :: Text) => Text -> Yaml.Object
volumeMount mountPath = toObj ANON{name = ?name, mountPath = mountPath}

workload ::
  (?app :: Text, ?name :: Text) =>
  (ToJSON spec, ToJSON podTemplateSpec) =>
  Text ->
  spec ->
  podTemplateSpec ->
  Yaml.Object
workload kind spec podTemplateSpec =
  [objQQ|
apiVersion: apps/v1
$object:  
spec:
  replicas: 1
  selector:
    matchLabels:
      app: $name
  template:
    metadata: $meta
    spec: $podTemplateSpec
  spec: $spec
|]
    ANON
      { object = object kind
      , name = ?name
      , meta = meta
      , podTemplateSpec = podTemplateSpec
      , spec = spec
      }

deployment :: (?app :: Text, ?name :: Text) => ToJSON spec => spec -> Yaml.Object
deployment = workload "Deployment" [objQQ| strategy: { type: Recreate } |]

statefulSet ::
  (?app :: Text, ?name :: Text) =>
  (ToJSON podTemplateSpec, ToJSON persistentVolumeClaim) =>
  podTemplateSpec ->
  [persistentVolumeClaim] ->
  Yaml.Object
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

issuer :: (?app :: Text) => [Yaml.Object]
issuer =
  [ [objQQ|
apiVersion: v1
kind: Secret
type: Opaque
metadata:
  name: cloudflare-origin-ca-key
  labels:
    app: $app
stringData:
  key: $key
|]
      ANON
        { app = ?app
        , key = Aeson.String cloudflareOriginCAKey
        }
  , [objQQ|
apiVersion: cert-manager.k8s.cloudflare.com/v1
kind: OriginIssuer
metadata:
  name: cloudflare-origin-issuer
  labels:
    app: $app
spec:
  requestType: OriginECC
  auth:
    serviceKeyRef:
      name: cloudflare-origin-ca-key
      key: key
|]
      ANON{app = ?app}
  ]

ingressContourTlsAnnotations :: Yaml.Object
ingressContourTlsAnnotations =
  [objQQ|
cert-manager.io/issuer: cloudflare-origin-issuer
cert-manager.io/issuer-kind: OriginIssuer
cert-manager.io/issuer-group: cert-manager.k8s.cloudflare.com
ingress.kubernetes.io/force-ssl-redirect: "true"
|]

ingressContourTls ::
  (?app :: Text, ?name :: Text) =>
  (AllFields r ToJSON, KnownFields r, RowHasField "host" r Text) =>
  [Record r] ->
  -- [ Record
  --     [ "host" := Text
  --     , "http"
  --         := Record
  --             '[ "paths"
  --                 := [ Record
  --                       [ "backend" := Record back
  --                       , "path" := Text
  --                       , "pathType" := PathType
  --                       ]
  --                    ]
  --              ]
  --     ]
  -- ] ->
  Yaml.Object
ingressContourTls rules =
  ingressContourTlsAnnotations
    <> KeyMap.insert "apiVersion" "networking.k8s.io/v1" (object "Ingress")
    <> [objQQ|
ingressClassName: contour
rules: $rules
tls:
- secretName: $name
  hosts: $hosts
|]
      ANON
        { rules = toJSON rules
        , name = ?name
        , hosts = view #host <$> rules
        }

ingressRule :: (?project :: Text, ?name :: Text) => Text -> Yaml.Object
ingressRule path =
  [objQQ|
host: $domain
http:
  paths:
  - backend: $backend
    path: $path
    pathType: Prefix
|]
    ANON
      { domain = domain
      , backend = backend
      , path = path
      }

backend :: (?name :: Text) => Yaml.Object
backend = toObj ANON{service = ANON{name = ?name, port = named}}

type Manifest = Record ["path" := FilePath, "objects" := [Yaml.Object]]

data YamlType
  = Manifest
  | HelmValues
      ( Record
          '[ "chart" := String -- name of chart, like "harbor/harbor"
           , "namespace" := Text
           , "appLabel" := Text
           ]
      )
  deriving (Show)

type Yaml =
  Record
    [ "yamlType" := YamlType
    , "value" := Yaml.Value
    ]

type HelmRow =
  [ "templates" := [Yaml.Object]
  , "crds" := [Yaml.Object]
  , "values" := Yaml.Object
  , "valuesSchema" := Yaml.Object
  , "chart" := Yaml.Object
  , "readme" := Text
  , "license" := Text
  , "helmignore" := Text
  ]

type Helm = Record HelmRow

type Project =
  Record
    [ "project" := Yaml.Object
    , "images" := [Yaml.Object]
    , "helm" := Helm
    ]

werfProject :: (?app :: Text) => Yaml.Object
werfProject =
  [objQQ|
project: $project
configVersion: 1
deploy:
  namespace: $project
|]
    ANON{project = ?app}

defaultHelm :: Helm
defaultHelm =
  ANON
    { templates = mempty
    , crds = mempty
    , values = mempty
    , valuesSchema = mempty
    , chart = mempty
    , readme = mempty
    , license = mempty
    , helmignore = mempty
    }

defineHelm :: SubRow HelmRow r => Record r -> Helm
defineHelm = flip inject defaultHelm

encodeAll :: [Yaml.Object] -> ByteString
encodeAll = foldl (\acc doc -> acc <> "---\n" <> doc <> "\n") mempty . fmap Yaml.encode

mkYaml :: YamlType -> Aeson.Value -> Yaml
mkYaml ty value =
  ANON
    { yamlType = ty
    , value = value
    }

manifest :: (?namespace :: Text, ?app :: Text) => ToJSON json => ((?name :: Text) => json) -> Yaml
manifest =
  let ?name = ?app
   in mkYaml Manifest
        . ( if ?namespace == noNamespace
              then id
              else over (key "metadata" % _Object) (KeyMap.insert "namespace" $ Aeson.String ?namespace)
          )
        . toJSON

helmValues ::
  (?namespace :: Text) =>
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
    . toJSON

$(deriveJSON ''PathType)