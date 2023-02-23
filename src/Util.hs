module Util (
  Application,
  Helm,
  Project,
  application,
  assignJSON,
  clusterIssuer,
  concatApplication,
  configMap,
  configMapVolume,
  container,
  containerPort,
  defineHelm,
  deployment,
  domain,
  emptyDirVolume,
  encodeAll,
  execCommandProbe,
  hostPathVolume,
  httpGetProbe,
  httpServicePort,
  ingressContourTls,
  ingressContourTlsAnnotations,
  ingressRule,
  issuerName,
  labelSelector,
  mergeObject,
  meta,
  mirror,
  name,
  named,
  noNamespace,
  nonrootGid,
  nonrootOwn,
  nonrootUid,
  openebsLvmClaim,
  openebsLvmProvisioner,
  persistentVolumeClaim,
  persistentVolumeClaimVolume,
  readWriteOnce,
  registry,
  rootGid,
  rootOwn,
  rootUid,
  secret,
  service,
  servicePort,
  setJSON,
  setSpecTo,
  statefulSet,
  systemClusterCritical,
  tcpSocketProbe,
  toObj,
  v1,
  v1Object,
  volumeMount,
  workload,
  cloudflareOriginCACertificate,
  image,
  nobodyGid,
  nobodyUid,
  nobodyOwn,
  directory,
) where

import Secret (cloudflareOriginCAKey, host)
import TH (deriveJSON, here, objQQ)

import Control.Arrow (first)
import Control.Monad.State.Strict (MonadState, modify)
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (AsValue (_Object))
import Data.ByteString (ByteString)
import Data.Record.Anon (AllFields, K (K), KnownFields, Merge, Proxy (..), RowHasField, SubRow, unI, pattern (:=))
import qualified Data.Record.Anon.Advanced as Record.Advanced
import Data.Record.Anon.Simple (Record, fromAdvanced, inject, insert, merge, toAdvanced)
import qualified Data.Record.Anon.Simple as Anon
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import Optics (A_Setter, Is, Optic', over, set, view)
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

setSpecTo :: Record v1Object -> spec -> Record (Merge v1Object '["spec" := spec])
setSpecTo v1Object spec = merge v1Object ANON{spec = spec}

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

domain :: (?subdomain :: Text) => Text
domain = ?subdomain <> "." <> host

named :: (?name :: Text) => Yaml.Object
named = toObj ANON{name = ?name}

labelSelector :: (?subdomain :: Text, ?app :: Text) => Yaml.Object
labelSelector =
  [objQQ|
selector:
  app: $?app
  subdomain: $?subdomain
|]

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

nobodyOwn :: Owner
nobodyOwn = (nobodyUid, nobodyGid)
nobodyUid :: UserID
nobodyUid = 65534
nobodyGid :: GroupID
nobodyGid = 65534

meta :: (?subdomain :: Text, ?app :: Text, ?name :: Text) => Yaml.Object
meta =
  [objQQ|
name: $?name
labels:
  app: $?app
  subdomain: $?subdomain
|]

object :: (?subdomain :: Text, ?app :: Text, ?name :: Text) => Text -> Text -> Yaml.Object
object ver kind =
  [objQQ|
apiVersion: $ver
kind: $kind
metadata: $meta
|]

v1Object :: (?subdomain :: Text, ?app :: Text, ?name :: Text) => Text -> Yaml.Object
v1Object = object "v1"

-- annotate ::
--   RowHasField "metadata" r (Record ObjectMeta) =>
--   KeyMap Text ->
--   Record r ->
--   Record _
-- annotate annotations object =
--   merge
--     object
--     ANON{metadata = Anon.get #metadata object `merge` ANON{annotations = annotations}}

configMap :: (?subdomain :: Text, ?app :: Text, ?name :: Text) => ToJSON d => d -> Yaml.Object
configMap d = v1Object "ConfigMap" <> [objQQ|{ immutable: true, data: $d }|]

container :: (?name :: Text) => Text -> Yaml.Object
container image =
  [objQQ|
name: $?name
image: $image
securityContext:
  allowPrivilegeEscalation: false
imagePullPolicy: Always
|]

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
--    in v1Object "Namespace"
--         `merge` ANON
--           { metadata = Anon.project meta :: Record ["name" := _, "labels" := _]
--           }

noNamespace :: Text
noNamespace = "_root"

persistentVolumeClaim :: (?subdomain :: Text, ?app :: Text, ?name :: Text) => ToJSON spec => spec -> Yaml.Object
persistentVolumeClaim spec = v1Object "PersistentVolumeClaim" <> toObj ANON{spec = spec}

readWriteOnce :: Text
readWriteOnce = "ReadWriteOnce"

openebsLvmClaim :: (?subdomain :: Text, ?app :: Text, ?name :: Text) => Text -> Yaml.Object
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

service :: (?subdomain :: Text, ?app :: Text, ?name :: Text) => ToJSON spec => spec -> Yaml.Object
service spec =
  v1Object "Service"
    <> toObj ANON{spec = over _Object (<> labelSelector) $ toJSON spec}

servicePort :: (?name :: Text) => Int -> Yaml.Object
servicePort port = toObj ANON{name = ?name, port = port, targetPort = ?name}

httpServicePort :: (?name :: Text) => Yaml.Object
httpServicePort = servicePort 80

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
  claimName: $?name
  readOnly: false
|]

volumeMount :: (?name :: Text) => FilePath -> Yaml.Object
volumeMount mountPath = toObj ANON{name = ?name, mountPath = mountPath}

workload ::
  (?subdomain :: Text, ?app :: Text, ?name :: Text) =>
  (ToJSON podTemplateSpec) =>
  Text ->
  Yaml.Object ->
  podTemplateSpec ->
  Yaml.Object
workload kind spec podTemplateSpec =
  [objQQ|
$workload:
spec:
  replicas: 1
  selector:
    matchLabels:
      app: $?app
  template:
    metadata: $meta
    spec: $podTemplateSpec
  $spec:
|]
 where
  workload = object "apps/v1" kind

deployment :: (?subdomain :: Text, ?app :: Text, ?name :: Text) => ToJSON spec => spec -> Yaml.Object
deployment = workload "Deployment" [objQQ| strategy: { type: Recreate } |]

statefulSet ::
  (?subdomain :: Text, ?app :: Text, ?name :: Text) =>
  (ToJSON podTemplateSpec, ToJSON persistentVolumeClaim) =>
  [persistentVolumeClaim] ->
  podTemplateSpec ->
  Yaml.Object
statefulSet persistentVolumeClaims podTemplateSpec =
  workload
    "StatefulSet"
    [objQQ|
serviceName: $?name
podManagementPolicy: Parallel
volumeClaimTemplatesa: $persistentVolumeClaims
|]
    podTemplateSpec

data PathType
  = Exact
  | Prefix
  | ImplementationSpecific
  deriving (Show)

clusterIssuer :: Text
clusterIssuer = "1inguini-ca-cluster-issuer"

issuerName :: Text
issuerName = "cloudflare-origin-ca"

cloudflareOriginCACertificate :: Text
cloudflareOriginCACertificate =
  [here|
-----BEGIN CERTIFICATE-----
MIICiTCCAi6gAwIBAgIUXZP3MWb8MKwBE1Qbawsp1sfA/Y4wCgYIKoZIzj0EAwIw
gY8xCzAJBgNVBAYTAlVTMRMwEQYDVQQIEwpDYWxpZm9ybmlhMRYwFAYDVQQHEw1T
YW4gRnJhbmNpc2NvMRkwFwYDVQQKExBDbG91ZEZsYXJlLCBJbmMuMTgwNgYDVQQL
Ey9DbG91ZEZsYXJlIE9yaWdpbiBTU0wgRUNDIENlcnRpZmljYXRlIEF1dGhvcml0
eTAeFw0xOTA4MjMyMTA4MDBaFw0yOTA4MTUxNzAwMDBaMIGPMQswCQYDVQQGEwJV
UzETMBEGA1UECBMKQ2FsaWZvcm5pYTEWMBQGA1UEBxMNU2FuIEZyYW5jaXNjbzEZ
MBcGA1UEChMQQ2xvdWRGbGFyZSwgSW5jLjE4MDYGA1UECxMvQ2xvdWRGbGFyZSBP
cmlnaW4gU1NMIEVDQyBDZXJ0aWZpY2F0ZSBBdXRob3JpdHkwWTATBgcqhkjOPQIB
BggqhkjOPQMBBwNCAASR+sGALuaGshnUbcxKry+0LEXZ4NY6JUAtSeA6g87K3jaA
xpIg9G50PokpfWkhbarLfpcZu0UAoYy2su0EhN7wo2YwZDAOBgNVHQ8BAf8EBAMC
AQYwEgYDVR0TAQH/BAgwBgEB/wIBAjAdBgNVHQ4EFgQUhTBdOypw1O3VkmcH/es5
tBoOOKcwHwYDVR0jBBgwFoAUhTBdOypw1O3VkmcH/es5tBoOOKcwCgYIKoZIzj0E
AwIDSQAwRgIhAKilfntP2ILGZjwajktkBtXE1pB4Y/fjAfLkIRUzrI15AiEA5UCL
XYZZ9m2c3fKwIenMMojL1eqydsgqj/wK4p5kagQ=
-----END CERTIFICATE-----
|]

secret :: (?subdomain :: Text, ?app :: Text, ?name :: Text) => Yaml.Object
secret = v1Object "Secret"

ingressContourTlsAnnotations :: Yaml.Object
ingressContourTlsAnnotations =
  [objQQ|
cert-manager.io/issuer: $issuerName
cert-manager.io/issuer-kind: OriginIssuer
cert-manager.io/issuer-group: cert-manager.k8s.cloudflare.com
ingress.kubernetes.io/force-ssl-redirect: "true"
|]

ingressContourTls ::
  (?subdomain :: Text, ?app :: Text, ?name :: Text) =>
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
  [objQQ|
$ingress:
$ingressContourTlsAnnotations:
ingressClassName: contour
rules: $rules
tls:
- secretName: $?name
  hosts: $hosts
|]
 where
  ingress = object "networking.k8s.io/v1" "Ingress"
  hosts = view #host <$> rules

ingressRule :: (?subdomain :: Text, ?project :: Text, ?name :: Text) => Text -> Yaml.Object
ingressRule path =
  [objQQ|
host: $domain
http:
  paths:
  - backend: $backend
    path: $path
    pathType: Prefix
|]

backend :: (?name :: Text) => Yaml.Object
backend = toObj ANON{service = ANON{name = ?name, port = named}}

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

type ApplicationRow =
  [ "images" := [Yaml.Object]
  , "helm" := Helm
  ]

type Application = Record ApplicationRow

type ProjectRow = "project" := Yaml.Object : ApplicationRow

type Project = Record ProjectRow

application ::
  SubRow ApplicationRow r =>
  Text ->
  ((?subdomain :: Text, ?app :: Text, ?name :: Text) => Record r) ->
  Application
application subdomain =
  let ?subdomain = subdomain
      ?app = subdomain
      ?name = subdomain
   in flip inject mempty

-- defaultHelm :: Helm
-- defaultHelm =
--   ANON
--     { templates = mempty
--     , crds = mempty
--     , values = mempty
--     , valuesSchema = mempty
--     , chart = mempty
--     , readme = mempty
--     , license = mempty
--     , helmignore = mempty
--     }

image :: (?name :: Text) => Yaml.Object
image = [objQQ|image: $?name|]

directory :: (?name :: Text) => FilePath
directory = "/" <> cs ?name

mergeYaml :: Yaml.Value -> Yaml.Value -> Yaml.Value
mergeYaml (Yaml.Array x) (Yaml.Array y) = Yaml.Array $ x <> y
mergeYaml (Yaml.Object x) (Yaml.Object y) = Yaml.Object $ mergeObject x y
mergeYaml x _ = x

mergeObject :: Yaml.Object -> Yaml.Object -> Yaml.Object
mergeObject = KeyMap.unionWith mergeYaml

appendApplication :: Application -> Application -> Application
appendApplication x y =
  ANON
    { images = view #images x <> view #images y
    , helm = appendHelm (view #helm x) (view #helm y)
    }
 where
  appendHelm x y =
    ANON
      { templates = view #templates x <> view #templates y
      , crds = view #crds x <> view #crds y
      , values = mergeObject (view #values x) (view #values y)
      , valuesSchema = mergeObject (view #valuesSchema x) (view #valuesSchema y)
      , chart = mergeObject (view #chart x) (view #chart y)
      , readme = view #readme x <> view #readme y
      , license = view #license x <> view #license y
      , helmignore = view #helmignore x <> view #helmignore y
      }

concatApplication :: [Application] -> Application
concatApplication = foldl appendApplication mempty

instance (AllFields r Semigroup, KnownFields r) => Semigroup (Record r) where
  (<>) x y =
    fromAdvanced $
      Record.Advanced.czipWith (Proxy :: Proxy Semigroup) (<>) (toAdvanced x) (toAdvanced y)

instance (AllFields r Semigroup, AllFields r Monoid, KnownFields r) => Monoid (Record r) where
  mempty = fromAdvanced $ Record.Advanced.cpure (Proxy :: Proxy Monoid) mempty

defineHelm :: SubRow HelmRow r => Record r -> Helm
defineHelm = flip inject mempty

encodeAll :: [Yaml.Object] -> ByteString
encodeAll = foldl (\acc doc -> acc <> "---\n" <> doc) mempty . fmap Yaml.encode

$(deriveJSON ''PathType)