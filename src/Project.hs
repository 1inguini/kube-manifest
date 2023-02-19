module Project (
  projects,
)
where

import Control.Monad.State.Strict (MonadState, execState, modify)
import Data.Aeson (toJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (AsJSON (_JSON), key, _Object)
import Data.FileEmbed (embedStringFile)
import Data.Record.Anon
import Data.Record.Anon.Simple (Record, inject, merge, project)
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
  Project,
  configMap,
  configMapVolume,
  containerPort,
  defaultHelm,
  defineHelm,
  deployment,
  httpGetProbe,
  httpServicePort,
  meta,
  nonrootGid,
  openebsLvmProvisioner,
  service,
  servicePort,
  systemClusterCritical,
  toObj,
  volumeMount,
  werfProject,
 )
import qualified Util

openebs :: Project
openebs =
  let ?app = "openebs"
   in ANON
        { project = werfProject
        , images = []
        , helm =
            defineHelm
              ANON
                { templates =
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
dns :: Project
dns =
  let ?app = "dns"
      ?name = "coredns"
   in let health = Util.name "health"
          ready = Util.name "ready"
          metrics = Util.name "metrics"
          mountPath = "/etc/coredns/"
       in ANON
            { project = werfProject
            , images = []
            , helm =
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

-- projectcontour :: [Yaml]
-- projectcontour =
--   let ?namespace = "projectcontour"
--       ?app = "contour"
--    in [ let ?app = "envoy"
--          in Util.manifest $
--               Util.service
--                 ANON
--                   { externalIPs = [Aeson.String externalIp]
--                   , ports =
--                       [yamlQQ|
--                         - name: http
--                           port: 80
--                           targetPort: 8080
--                         - name: https
--                           port: 443
--                           targetPort: 8443
--                     |]
--                   }
--       , Util.manifest
--           $ Util.annotate
--             (KeyMap.singleton "ingressclass.kubernetes.io/is-default-class" "true")
--           $ Util.setSpecTo
--             (Anon.set #apiVersion "networking.k8s.io/v1" $ Util.object "IngressClass")
--             ANON{controller = "projectcontour.io/contour" :: Text}
--       ]

-- kubernetesDashboard :: [Yaml]
-- kubernetesDashboard =
--   let ?namespace = "kubernetes-dashboard"
--       ?app = "kubernetes-dashboard"
--    in [ Util.manifest $
--           let ?name = "admin-user"
--            in Util.object "ServiceAccount"
--       , Util.manifest
--           [yamlQQ|
--             apiVersion: rbac.authorization.k8s.io/v1
--             kind: ClusterRoleBinding
--             metadata:
--               name: admin-user
--               labels:
--                 app: kubernetes-dashboard
--             roleRef:
--               apiGroup: rbac.authorization.k8s.io
--               kind: ClusterRole
--               name: cluster-admin
--             subjects:
--               - kind: ServiceAccount
--                 name: admin-user
--                 namespace: kubernetes-dashboard
--           |]
--       ]

-- registry :: [Yaml]
-- registry =
--   let ?namespace = "registry"
--       ?app = "harbor"
--    in Util.issuer
--         <> [ Util.helmValues
--               ANON{chart = "harbor/harbor", appLabel = "app"}
--               $ [yamlQQ|
--               expose:
--                 ingress:
--                   annotations: $annotations
--                   hosts:
--                     core: $domain
--                     notary: $notary
--                 tls:
--                   certSource: secret
--                   secret:
--                     secretName: $app
--                     notarySecretName: $notarySecret
--               caSecretName: $app
--               externalURL: $url
--               updateStrategy:
--                 type: Recreate
--             |]
--                 ANON
--                   { annotations = Util.ingressContourTlsAnnotations
--                   , app = Aeson.String ?app
--                   , domain = Util.domain
--                   , notary = "notary." <> Util.domain
--                   , notarySecret = "notary-" <> ?app
--                   , url = "https://" <> Util.domain
--                   }
--            ]

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

projects :: [Project]
projects =
  [ openebs
  , dns
  ]