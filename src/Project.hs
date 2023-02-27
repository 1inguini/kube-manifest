module Project (projects) where

import Data.Aeson.KeyMap as KeyMap
import Data.FileEmbed (embedStringFile)
import qualified Data.Record.Anon.Simple as Anon
import Data.Text (Text)
import Data.Yaml as Yaml
import System.FilePath ((</>))

import Secret
import TH (embedFromYamlAllFile, embedYamlAllFile, objQQ)
import Util (
  Project,
  cloudflareOriginCACertificate,
  clusterDomain,
  configMap,
  configMapVolume,
  containerPort,
  defineHelm,
  deployment,
  directory,
  domain,
  httpGetProbe,
  httpServicePort,
  image,
  ingressContourTlsAnnotations,
  issuer,
  issuerName,
  labels,
  localIssuerName,
  meta,
  openebsLvmProvisioner,
  secret,
  service,
  servicePort,
  systemClusterCritical,
  toObj,
  volumeMount,
  werfProject,
 )
import qualified Util

nonroot :: Project
nonroot =
  werfProject
    "nonroot"
    $ ANON
      { images =
          [ [objQQ|
$image:
git:
- add: $directory
  to: /etc
docker:
  USER: nonroot
  WORKDIR: /home/nonroot
|]
          ]
      }

openebs :: Project
openebs =
  werfProject
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
dns :: Project
dns =
  werfProject "dns" $
    let ?name = "coredns"
        ?app = "coredns"
     in let health = Util.name "health"
            ready = Util.name "ready"
            metrics = Util.name "metrics"
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

postgresql :: Project
postgresql =
  werfProject
    "cnpg-system"
    ANON
      { helm =
          defineHelm
            ANON
              { chart =
                  [objQQ|
apiVersion: v2
dependencies:
- name: cloudnative-pg
  version: ~0.17.0
  repository: https://cloudnative-pg.github.io/charts
|]
              }
      }

projectcontour :: Project
projectcontour =
  werfProject "projectcontour" $
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

certManager :: Project
certManager =
  werfProject
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
                  let ?name = localIssuerName
                   in [ [objQQ|
apiVersion: cert-manager.io/v1
kind: ClusterIssuer
metadata: $meta
spec:
  selfSigned: {}
|]
                      , [objQQ|
apiVersion: cert-manager.io/v1
kind: Certificate
metadata: $meta
spec:
  duration: 2160h # 90d
  renewBefore: 360h # 15d
  isCA: true
  secretName: $?name
  privateKey:
    algorithm: ECDSA
    size: 256
  dnsNames:
  - $clusterDomain
  issuerRef:
    name: $?name
    kind: ClusterIssuer
    group: cert-manager.io
|]
                      ]
              }
      }

kubernetesDashboard :: Project
kubernetesDashboard =
  werfProject "kubernetes-dashboard" $
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
  settings:
    itemsPerPage: 100
    labelsLimit: 3
    # Number of seconds between every auto-refresh of logs
    logsAutoRefreshTimeInterval: 5
    # Number of seconds between every auto-refresh of every resource. Set 0 to disable
    resourceAutoRefreshTimeInterval: 5
    # Hide all access denied warnings in the notification panel
    disableAccessDeniedNotifications: false
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
    namespace: $projectName
|]
                      ]
                  }
          }

authentication :: Project
authentication =
  let db = "database"
      dbService = db <> "-rw"
      dbUser = db <> "-app"
      dbAdmin = db <> "-superuser"
   in werfProject
        "auth"
        $ let ?app = "zitadel"
           in ANON
                { helm =
                    defineHelm
                      ANON
                        { chart =
                            [objQQ|
apiVersion: v2
dependencies:
- name: zitadel
  version: ~4.1.4
  repository: https://charts.zitadel.com
  |]
                        , values =
                            [objQQ|
zitadel:
  zitadel:
    configmapConfig:
      ExternalSecure: true
      ExternalDomain: $domain
      Database:
        postgres:
          Host: $dbService
          Port: 5432
          Database: zitadel
          MaxOpenConns: 20
          MaxConnLifetime: 30m
          MaxConnIdleTime: 30m
          Options: ""
          User:
            SSL:
              Mode: enable
              RootCert: /.secrets/ca.crt
              Cert: /.secrets/tls.crt
              Key: /.secrets/tls.key
          Admin:
            SSL:
              Mode: enable
              RootCert: /.secrets/ca.crt
              Cert: /.secrets/tls.crt
              Key: /.secrets/tls.key
      Machine:
        Identification:
          Hostname:
            Enabled: true
          Webhook:
            Enabled: false
    masterkey: $zitadelMasterkey
    dbSslRootCrtSecret: $db
    dbSslClientCrtSecret: $db
  ingress:
    enabled: true
    annotation: $ingressContourTlsAnnotations
    hosts:
      - host: $domain
        paths: /
        pathType: Prefix
  env:
  - name: ZITADEL_DATABASE_POSTGRES_USER_USERNAME
    valueFrom:
      secretKeyRef:
        name: $dbUser
        key: username
  - name: ZITADEL_DATABASE_POSTGRES_USER_PASSWORD
    valueFrom:
      secretKeyRef:
        name: $dbUser
        key: password
  - name: ZITADEL_DATABASE_POSTGRES_ADMIN_USERNAME
    valueFrom:
      secretKeyRef:
        name: $dbAdmin
        key: username
  - name: ZITADEL_DATABASE_POSTGRES_ADMIN_PASSWORD
    valueFrom:
      secretKeyRef:
        name: $dbAdmin
        key: password
  # metrics:
  #   enabled: true
  #   serviceMonitor:
  #     enabled: true
|]
                        , templates =
                            issuer
                              <> Util.name
                                db
                                [ [objQQ|
$secret:
metadata:
  labels:
    cnpg.io/reload: ""
|]
                                , [objQQ|
apiVersion: cert-manager.io/v1
kind: Certificate
metadata: $meta
spec:
  secretName: $?name
  usages:
    - client auth
  commonName: streaming_replica
  issuerRef:
    group: cert-manager.io
    kind: ClusterIssuer
    name: $localIssuerName
|]
                                , [objQQ|
apiVersion: postgresql.cnpg.io/v1
kind: Cluster
metadata: $meta
spec:
  instances: 3
  certificates:
    clientCASecret: $?name
    replicationTLSSecret: $?name
  bootstrap:
    initdb:
      database: zitadel
      owner: zitadel
  storage:
    size: 1Gi
|]
                                ]
                        }
                }

registry :: Project
registry =
  werfProject "registry" $
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
                      , templates = issuer
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

projectName :: Text
projectName = "oneinguini"

projects :: [Project]
projects =
  [ authentication
  , certManager
  , postgresql
  , dns
  , kubernetesDashboard
  , openebs
  , projectcontour
  , registry
  ]