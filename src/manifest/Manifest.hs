module Manifest where

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

import qualified Data.Aeson.KeyMap as KeyMap
import Data.Yaml.TH (yamlQQ)
import Secret
import TH (yamlExp)
import Text.Heredoc (here)
import Util (nonrootGid)
import Util.Manifest (Yaml)
import qualified Util.Manifest as Util

openebs :: [Yaml]
openebs =
  let ?name = Util.noNamespace
      ?app = "openebs"
   in [ Util.manifest
          [yamlQQ|
            apiVersion: storage.k8s.io/v1
            kind: StorageClass
            metadata:
              name: openebs-lvmpv
              labels:
                app: openebs
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

certManager :: [Yaml]
certManager =
  let ?namespace = "cert-manager"
      ?app = "cert-manager"
   in [ Util.manifest $
          $( yamlExp
              ["_token"]
              [here|
                apiVersion: v1
                kind: Secret
                type: Opaque
                metadata:
                  namespace: cert-manager
                  name: cloudflare-origin-ca-key
                  labels:
                    app: cert-manager
                stringData:
                  api-token: _token
              |]
           )
            ANON{_token = Aeson.String cloudflareOriginCAKey}
      , Util.manifest
          [yamlQQ|
            apiVersion: cert-manager.io/v1
            kind: ClusterIssuer
            metadata:
              # namespace: cert-manager
              name: 1inguini-ca-cluster-issuer
              labels:
                app: cert-manager
            spec:
              acme:
                # You must replace this email address with your own.
                # Let's Encrypt will use this to contact you about expiring
                # certificates, and issues related to your account.
                email: 9647142@gmail.com
                server: https://acme-staging-v02.api.letsencrypt.org/directory
                privateKeySecretRef:
                  # Secret resource that will be used to store the account's private key.
                  name: letsencrypt-issuer-account-key
                solvers:
                - http01:
                    ingress: {}
                # - dns01:
                #     cloudflare:
                #       apiTokenSecretRef:
                #         name: cloudflare-api-token
                #         key: api-token
          |]
          -- , Util.manifest
          --     [yamlQQ|
          --       apiVersion: cert-manager.io/v1
          --       kind: Certificate
          --       metadata:
          --         namespace: cert-manager
          --         name: 1inguini-ca
          --         labels:
          --           app: cert-manager
          --       spec:
          --         duration: 2160h # 90d
          --         renewBefore: 360h # 15d
          --         isCA: true
          --         secretName: 1inguini-ca
          --         privateKey:
          --           algorithm: ECDSA
          --           size: 256
          --         dnsNames:
          --         - 1inguini.com
          --         issuerRef:
          --           name: letsencrypt-issuer
          --           kind: Issuer
          --           group: cert-manager.io
          --     |]
          -- , Util.manifest
          --     [yamlQQ|
          --       apiVersion: cert-manager.io/v1
          --       kind: ClusterIssuer
          --       metadata:
          --         name: 1inguini-ca-cluster-issuer
          --         labels:
          --           app: cert-manager
          --       spec:
          --         ca:
          --           secretName: 1inguini-ca
          --     |]
      ]

-- config for coredns
dns :: [Yaml]
dns =
  let ?namespace = "dns"
      ?app = "coredns"
   in let mountPath = "/etc/coredns/"
          health = Util.name "health"
          ready = Util.name "ready"
          metrics = Util.name "metrics"
       in [ Util.manifest Util.namespace
          , Util.manifest $
              Util.deployment
                ANON
                  { containers =
                      [ Util.container
                          "registry.k8s.io/coredns/coredns:v1.9.3"
                          ANON
                            { args = ["-conf", mountPath <> "Corefile"]
                            , command = ["/coredns"] :: [Text]
                            , ports =
                                ( Util.containerPort 53 `merge` ANON{protocol = "UDP" :: Text}
                                , metrics $ Util.containerPort 9153
                                , health $ Util.containerPort 8080
                                , ready $ Util.containerPort 8081
                                )
                            , livenessProbe = health $ Util.httpGetProbe "health"
                            , readinessProbe = ready $ Util.httpGetProbe "ready"
                            , securityContext =
                                ANON
                                  { capabilities =
                                      [yamlQQ|
                                        add:
                                          - NET_BIND_SERVICE
                                        drop:
                                          - all
                                      |]
                                  , readOnlyRootFilesystem = True
                                  }
                            , volumeMounts = [Util.volumeMount mountPath]
                            }
                      ]
                  , priorityClassName = Util.systemClusterCritical
                  , volumes = [Util.configMapVolume]
                  }
          , Util.manifest $
              Util.configMap (KeyMap.singleton "Corefile" ($(embedStringFile "src/manifest/dns/Corefile") :: Text))
          , Util.manifest $
              Util.service $
                ANON
                  { externalIPs = [Aeson.String externalIp]
                  , ports = [Util.servicePort 53 `merge` ANON{protocol = [yamlQQ|UDP|]}]
                  }
          , Util.manifest $
              metrics $
                Util.service $
                  ANON{ports = [Util.servicePort 80]}
          ]

projectcontour :: [Yaml]
projectcontour =
  let ?namespace = "projectcontour"
      ?app = "contour"
   in [ let ?app = "envoy"
         in Util.manifest $
              Util.service
                ANON
                  { externalIPs = [Aeson.String externalIp]
                  , ports =
                      [yamlQQ|
                        - name: http
                          port: 80
                          targetPort: 8080
                        - name: https
                          port: 443
                          targetPort: 8443
                    |]
                  }
      , Util.manifest
          $ Util.annotate
            (KeyMap.singleton "ingressclass.kubernetes.io/is-default-class" "true")
          $ Util.setSpecTo
            (Anon.set #apiVersion "networking.k8s.io/v1" $ Util.object "IngressClass")
            ANON{controller = "projectcontour.io/contour" :: Text}
      ]

kubernetesDashboard :: [Yaml]
kubernetesDashboard =
  let ?namespace = "kubernetes-dashboard"
      ?app = "kubernetes-dashboard"
   in [ Util.manifest $
          let ?name = "admin-user"
           in Util.object "ServiceAccount"
      , Util.manifest
          [yamlQQ|
            apiVersion: rbac.authorization.k8s.io/v1
            kind: ClusterRoleBinding
            metadata:
              name: admin-user
              labels:
                app: kubernetes-dashboard
            roleRef:
              apiGroup: rbac.authorization.k8s.io
              kind: ClusterRole
              name: cluster-admin
            subjects:
              - kind: ServiceAccount
                name: admin-user
                namespace: kubernetes-dashboard
          |]
      ]

registry :: [Yaml]
registry =
  let ?namespace = "registry"
      ?app = "harbor"
   in [ Util.helmValues
          ANON{chart = "harbor/harbor", appLabel = "app"}
          $ $( yamlExp
                [ "_annotations"
                , "_domain"
                , "_notary"
                , "_app"
                , "_notarySecret"
                , "_url"
                ]
                [here|
                    expose:
                      ingress:
                        annotations: _annotations
                        hosts:
                          core: _domain
                          notary: _notary
                      tls:
                        certSource: secret
                        secret:
                          secretName: _app
                          notarySecretName: _notarySecret
                    caSecretName: _app
                    externalURL: _url
                    updateStrategy:
                      type: Recreate
                  |]
             )
            ANON
              { _annotations = Util.ingressContourTlsAnnotations
              , _domain = Util.domain
              , _notary = "notary." <> Util.domain
              , _app = Aeson.String ?app
              , _notarySecret = "notary-" <> ?app
              , _url = "https://" <> Util.domain
              }
      ]

gitbucket :: [Yaml]
gitbucket =
  let ?namespace = "git"
      ?app = "gitbucket"
   in let home = "home"
          homeInit = "home-init"
          homePath = "/home/nonroot/.gitbucket/"
          database = "mariadb"
          databaseDataPath = "/var/lib/mysql/"
          mariadbVersion = "latest" -- "10.9.4-2"
          gitbucketVersion = "latest" -- 4.38.4
       in [ Util.manifest Util.namespace
          , Util.manifest $
              Util.statefulSet
                ANON
                  { initContainers =
                      [ toJSON $
                          Util.name homeInit $
                            Util.container
                              (Util.registry <> "gitbucket/home-init:latest")
                              ANON{volumeMounts = [Util.name home $ Util.volumeMount "/mnt/"]}
                      ]
                  , containers =
                      [ toJSON $
                          Util.container
                            (Util.registry <> "gitbucket/main:" <> gitbucketVersion)
                            ANON
                              { ports = [Util.containerPort 8080]
                              , livenessProbe = Util.httpGetProbe "/api/v3"
                              , readinessProbe = Util.httpGetProbe "/api/v3"
                              , volumeMounts =
                                  [ toJSON $ Util.volumeMount homePath
                                  , toJSON $
                                      Util.name home $
                                        Util.volumeMount (homePath <> "plugins")
                                          `merge` ANON{subPath = "plugins" :: Text}
                                  , toJSON $
                                      Util.name home $
                                        Util.volumeMount (homePath <> "database.conf")
                                          `merge` ANON{subPath = "database.conf" :: Text}
                                  , toJSON $
                                      Util.name database $
                                        Util.volumeMount databaseDataPath
                                          `merge` ANON{subPath = "upperdir" :: Text}
                                  ]
                              }
                      , toJSON $
                          Util.name (database <> "-data") $
                            Util.container
                              (Util.registry <> "gitbucket/mariadb-datadir:" <> mariadbVersion)
                              ANON
                                { securityContext = ANON{privileged = True}
                                , lifecycle =
                                    ANON
                                      { preStop =
                                          ANON{exec = ANON{command = ["umount", "/mnt/upperdir"] :: [Text]}}
                                      }
                                , readinessProbe = Util.execCommandProbe ["test", "-e", "/mnt/upperdir/test"]
                                , volumeMounts =
                                    [ Util.name database $
                                        Util.volumeMount "/mnt"
                                          `merge` ANON{mountPropagation = "Bidirectional" :: Text}
                                    ]
                                }
                      ]
                  , volumes =
                      [ toJSON Util.persistentVolumeClaimVolume
                      , toJSON $ Util.name home Util.emptyDirVolume
                      , toJSON $ Util.name database Util.persistentVolumeClaimVolume
                      ]
                  , securityContext = ANON{fsGroup = fromEnum nonrootGid}
                  }
                [ Util.openebsLvmClaim "5Gi"
                , Util.name database $ Util.openebsLvmClaim "1Gi"
                ]
          , Util.manifest $ Util.service ANON{ports = [Util.httpServicePort]}
          , Util.manifest $ Util.ingressContourTls [Util.ingressRule "/"]
          ]

yamls :: [Yaml]
yamls =
  mconcat
    [ certManager
    , dns
    , gitbucket
    , kubernetesDashboard
    , openebs
    , projectcontour
    , registry
    ]