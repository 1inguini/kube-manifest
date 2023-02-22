module Project (
  project,
)
where

import Control.Monad.State.Strict (MonadState, execState, modify)
import Data.Aeson (toJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (AsJSON (_JSON), key, _Object)
import Data.FileEmbed (embedStringFile)
import Data.Record.Anon
import Data.Record.Anon.Simple (Record, inject, merge)
import qualified Data.Record.Anon.Simple as Anon
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding.Base64 (encodeBase64)
import Optics (ix, modifying, over, review, set, (%))
import System.FilePath ((</>))

import qualified Data.Aeson.KeyMap as KeyMap
import Secret
import TH (objQQ)
import Text.Heredoc (here)
import Util (
  Application,
  Helm,
  Project,
  application,
  cloudflareOriginCACertificate,
  concatApplication,
  configMap,
  configMapVolume,
  containerPort,
  defineHelm,
  deployment,
  domain,
  httpGetProbe,
  httpServicePort,
  ingressContourTlsAnnotations,
  issuerName,
  meta,
  openebsLvmProvisioner,
  secret,
  service,
  servicePort,
  systemClusterCritical,
  toObj,
  volumeMount,
 )
import qualified Util

openebs :: Application
openebs =
  application
    "openebs"
    ANON
      { helm =
          defineHelm
            ANON
              { chart =
                  [objQQ|
apiVersion: v2
dependencies:
- name: lvm-localpv 
  version: ~1.0.1
  repository: https://openebs.github.io/lvm-localpv
|]
              , templates =
                  [ let ?name = openebsLvmProvisioner
                     in [objQQ|
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  $meta:
  annotations:
    storageclass.kubernetes.io/is-default-class: "true"
allowVolumeExpansion: true
parameters:
  storage: "lvm"
  volgroup: "openebs"
  fstype: "ext4"
provisioner: local.csi.openebs.io
|]
                  ]
              }
      }

-- config for coredns
dns :: Application
dns =
  application "coredns" $
    let health = Util.name $ ?app <> "-" <> "health"
        ready = Util.name $ ?app <> "-" <> "ready"
        metrics = Util.name $ ?app <> "-" <> "metrics"
        mountPath = "/etc/coredns/"
     in ANON
          { helm =
              defineHelm
                ANON
                  { templates =
                      [ let coredns = Util.container "registry.k8s.io/coredns/coredns:v1.9.3"
                            coreFilePath = mountPath </> "Corefile"
                            corednsConf =
                              toObj
                                ANON
                                  { ports =
                                      [ containerPort 53 <> [objQQ|protocol: UDP|]
                                      , metrics $ containerPort 9153
                                      , health $ containerPort 8080
                                      , ready $ containerPort 8081
                                      ]
                                  , livenessProbe = health $ httpGetProbe "health"
                                  , readinessProbe = ready $ httpGetProbe "ready"
                                  , volumeMounts = [volumeMount mountPath]
                                  }
                         in [objQQ|
$deployment:
  containers:
  - $coredns:
    args:
    - -conf
    - $coreFilePath
    command:
    - /coredns
    securityContext:
      capabilities:
        add:
        - NET_BIND_SERVICE
        drop:
        - all
      readOnlyRootFilesystem: true
    $corednsConf:
  priorityClassName: $systemClusterCritical
  volumes:
  - $configMapVolume
|]
                      , let port = servicePort 53
                         in [objQQ|
$service:
  externalIPs:
  - $externalIp
  ports:
  - $port:
    protocol: UDP
|]
                      , configMap
                          (KeyMap.singleton "Corefile" ($(embedStringFile "src/dns/Corefile") :: Text))
                      , metrics
                          [objQQ|
$service:
  ports:
  - $httpServicePort
|]
                      ]
                  }
          }

projectcontour :: Application
projectcontour =
  application "projectcontour" $
    let ?app = "contour"
     in ANON
          { helm =
              defineHelm
                ANON
                  { chart =
                      [objQQ|
apiVersion: v2
dependencies:
- name: contour
  version: ~11.0.0
  repository: https://charts.bitnami.com/bitnami
|]
                  , values =
                      let envoyMeta = let ?name = "envoy" in meta
                       in [objQQ|
contour:
  envoy:
    service:
      type: ClusterIP
      $envoyMeta:
      externalTrafficPolicy: false
      externalIPs:
      - $externalIp
      targetPorts:
        http: 8080
        https: 8443
|]
                  }
          }

certManager :: Application
certManager =
  application
    "cert-manager"
    ANON
      { helm =
          defineHelm
            ANON
              { chart =
                  [objQQ|
apiVersion: v2
dependencies:
- name: cert-manager
  version: ~1.11.0
  repository: https://charts.jetstack.io
|]
              , values =
                  [objQQ|
cert-manager:
  installCRDs: true
|]
              , templates =
                  let ?name = issuerName
                   in [ [objQQ|
$secret:
type: Opaque
stringData:
  key: $cloudflareOriginCAKey
  ca.crt: $cloudflareOriginCACertificate
|]
                      , [objQQ|
apiVersion: cert-manager.k8s.cloudflare.com/v1
kind: OriginIssuer
metadata: $meta
spec:
  requestType: OriginECC
  auth:
    serviceKeyRef:
      name: $?name
      key: key
|]
                      ]
              }
      }

kubernetesDashboard :: Application
kubernetesDashboard =
  application "kubernetes-dashboard" $
    let admin = "cluster-admin"
     in ANON
          { helm =
              defineHelm
                ANON
                  { chart =
                      [objQQ|
apiVersion: v2
dependencies:
- name: kubernetes-dashboard
  version: ~6.0.0
  repository: https://kubernetes.github.io/dashboard
|]
                  , values =
                      [objQQ|
kubernetes-dashboard:
  rbac:
    clusterReadOnlyRole: true
  serviceAccount:
    name: $admin
|]
                  , templates =
                      [ let ?name = admin
                         in [objQQ|
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata: $meta
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: ClusterRole
  name: cluster-admin
subjects:
  - kind: ServiceAccount
    name: $admin
    namespace: kubernetes-dashboard
|]
                      ]
                  }
          }

registry :: Application
registry =
  application "registry" $
    let ?app = "harbor" :: Text
        ?name = ?app
     in let notaryDomain = "notary." <> domain
            notarySecret = ?app <> "-notary"
            coreSecret = ?app
            url = "https://" <> domain
         in ANON
              { helm =
                  defineHelm
                    ANON
                      { chart =
                          [objQQ|
apiVersion: v2
dependencies:
- name: harbor
  version: ~1.11.0
  repository: https://helm.goharbor.io
|]
                      , values =
                          [objQQ|
harbor:
  expose:
    type: ingress
    ingress:
      annotations: $ingressContourTlsAnnotations
      hosts:
        core: $domain
        notary: $notaryDomain
    tls:
      certSource: secret
      secret:
        secretName: $coreSecret
        notarySecretName: $notarySecret
    internalTLS:
      enabled: false
  caBundleSecretName: $issuerName
  caSecretName: $issuerName
  externalURL: $url
  updateStrategy:
    type: Recreate
  notary:
    enabled: false
|]
                      }
              }

-- gitbucket :: [Yaml]
-- gitbucket =
--   let ?namespace = "git"
--       ?app = "gitbucket"
--    in let home = "home"
--           homeInit = "home-init"
--           homePath = "/home/nonroot/.gitbucket/"
--           database = "mariadb"
--           databaseDataPath = "/var/lib/mysql/"
--           mariadbVersion = "latest" -- "10.9.4-2"
--           gitbucketVersion = "latest" -- 4.38.4
--        in Util.issuer
--             <> [ Util.manifest Util.namespace
--                , Util.manifest $
--                   Util.statefulSet
--                     ANON
--                       { initContainers =
--                           [ toJSON $
--                               Util.name homeInit $
--                                 Util.container
--                                   (Util.registry <> "gitbucket/home-init:latest")
--                                   ANON{volumeMounts = [Util.name home $ Util.volumeMount "/mnt/"]}
--                           ]
--                       , containers =
--                           [ toJSON $
--                               Util.container
--                                 (Util.registry <> "gitbucket/main:" <> gitbucketVersion)
--                                 ANON
--                                   { ports = [Util.containerPort 8080]
--                                   , livenessProbe = Util.httpGetProbe "/api/v3"
--                                   , readinessProbe = Util.httpGetProbe "/api/v3"
--                                   , volumeMounts =
--                                       [ toJSON $ Util.volumeMount homePath
--                                       , toJSON $
--                                           Util.name home $
--                                             Util.volumeMount (homePath <> "plugins")
--                                               `merge` ANON{subPath = "plugins" :: Text}
--                                       , toJSON $
--                                           Util.name home $
--                                             Util.volumeMount (homePath <> "database.conf")
--                                               `merge` ANON{subPath = "database.conf" :: Text}
--                                       , toJSON $
--                                           Util.name database $
--                                             Util.volumeMount databaseDataPath
--                                               `merge` ANON{subPath = "upperdir" :: Text}
--                                       ]
--                                   }
--                           , toJSON $
--                               Util.name (database <> "-data") $
--                                 Util.container
--                                   (Util.registry <> "gitbucket/mariadb-datadir:" <> mariadbVersion)
--                                   ANON
--                                     { securityContext = ANON{privileged = True}
--                                     , lifecycle =
--                                         ANON
--                                           { preStop =
--                                               ANON{exec = ANON{command = ["umount", "/mnt/upperdir"] :: [Text]}}
--                                           }
--                                     , readinessProbe = Util.execCommandProbe ["test", "-e", "/mnt/upperdir/test"]
--                                     , volumeMounts =
--                                         [ Util.name database $
--                                             Util.volumeMount "/mnt"
--                                               `merge` ANON{mountPropagation = "Bidirectional" :: Text}
--                                         ]
--                                     }
--                           ]
--                       , volumes =
--                           [ toJSON Util.persistentVolumeClaimVolume
--                           , toJSON $ Util.name home Util.emptyDirVolume
--                           , toJSON $ Util.name database Util.persistentVolumeClaimVolume
--                           ]
--                       , securityContext = ANON{fsGroup = fromEnum nonrootGid}
--                       }
--                     [ Util.openebsLvmClaim "5Gi"
--                     , Util.name database $ Util.openebsLvmClaim "1Gi"
--                     ]
--                , Util.manifest $ Util.service ANON{ports = [Util.httpServicePort]}
--                , Util.manifest $ Util.ingressContourTls [Util.ingressRule "/"]
--                ]

-- mconcat
--   [ dns
--   , gitbucket
--   , kubernetesDashboard
--   , openebs
--   , projectcontour
--   , registry
--   ]

project :: Project
project =
  Anon.applyPending
    $ Anon.insert
      #project
      [objQQ|
configVersion: 1
project: oneinguini
|]
    $ concatApplication
      [ certManager
      , dns
      , kubernetesDashboard
      , openebs
      , projectcontour
      , registry
      ]