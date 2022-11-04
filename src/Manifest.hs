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
import Optics (ix, modifying, over, review, set, (%))

import qualified Data.Aeson.KeyMap as KeyMap
import Secret (externalIp, host)
import TH (embedModifedYamlFile, embedYamlAllFile, embedYamlFile)
import Util (Yaml)
import qualified Util

openebs :: [Yaml]
openebs =
  let ?namespace = Util.noNamespace
      ?app = "openebs"
   in [Util.manifest $(embedYamlFile "src/openebs/storage-class.yaml")]

certManager :: [Yaml]
certManager =
  let ?namespace = "cert-manager"
      ?app = "cert-manager"
   in [ Util.manifest $(embedYamlFile "src/cert-manager/selfsigned-issuer.yaml")
      , Util.manifest $(embedYamlFile "src/cert-manager/1inguini-ca.yaml")
      , Util.manifest $(embedYamlFile "src/cert-manager/1inguini-ca-cluster-issuer.yaml")
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
                            { args = ["-conf", mountPath <> "Corefile"] :: [Text]
                            , command = ["/coredns"] :: [Text]
                            , ports =
                                [ toJSON $ Util.containerPort 53 `merge` ANON{protocol = "UDP" :: Text}
                                , toJSON $ metrics $ Util.containerPort 9153
                                , toJSON $ health $ Util.containerPort 8080
                                , toJSON $ ready $ Util.containerPort 8081
                                ]
                            , livenessProbe = health $ Util.probe $ Util.httpGet "health"
                            , readinessProbe = ready $ Util.probe $ Util.httpGet "ready"
                            , securityContext =
                                ANON
                                  { capabilities =
                                      ANON
                                        { add = ["NET_BIND_SERVICE"] :: [Text]
                                        , drop = ["all"] :: [Text]
                                        }
                                  , readOnlyRootFilesystem = True
                                  }
                            , volumeMounts = [Util.volumeMount mountPath]
                            }
                      ]
                  , priorityClassName = Util.systemClusterCritical
                  , volumes = [Util.configMapVolume]
                  }
          , Util.manifest $
              Util.configMap (KeyMap.singleton "Corefile" ($(embedStringFile "src/dns/Corefile") :: Text))
          , Util.manifest $
              Util.service $
                ANON
                  { externalIPs = [externalIp]
                  , ports =
                      [ toJSON $
                          Util.servicePort 53
                            `merge` ANON{protocol = "UDP" :: Text}
                      ]
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
   in [ Util.manifest $
          let ?app = "envoy"
              ?name = "envoy"
           in Util.service
                ANON
                  { externalIPs = [externalIp]
                  , ports =
                      [ ANON{name = "http" :: Text, port = 80 :: Int, targetPort = 8080 :: Int}
                      , ANON{name = "https" :: Text, port = 443 :: Int, targetPort = 8443 :: Int}
                      ]
                  }
      , Util.manifest
          $ Util.annotate
            (KeyMap.singleton "ingressclass.kubernetes.io/is-default-class" "true")
          $ Util.setSpecTo
            (Anon.set #apiVersion "networking.k8s.io/v1" $ Util.object "IngressClass")
            ANON{controller = "projectcontour.io/contour" :: Text}
      ]

registry :: [Yaml]
registry =
  let ?namespace = "registry"
      ?app = "harbor"
   in [ Util.helmValues
          "harbor/harbor"
          ANON
            { expose =
                ANON
                  { ingress =
                      ANON
                        { annotations = Util.ingressContourTlsAnnotations
                        , hosts =
                            ANON
                              { core = Util.domain
                              , notary = "notary." <> Util.domain
                              }
                        }
                  , tls =
                      ANON
                        { certSource = "secret" :: Text
                        , secret =
                            ANON
                              { secretName = ?app
                              , notarySecretName = "notary-" <> ?app
                              } -- ANON{auto = ANON{commonName = Util.domain}}
                        }
                  }
            , caSecretName = ?app
            , externalURL = "https://" <> Util.domain
            , updateStrategy = Anon.insert #type ("Recreate" :: Text) ANON{}
            }
      ]

gitbucket :: [Yaml]
gitbucket =
  let ?namespace = "git" :: Text
      ?app = "gitbucket" :: Text
   in let plugins = Util.name "plugins"
          registry = "registry." <> host <> "/library/"
          pluginsDir = "/home/nonroot/.gitbucket/plugins"
       in [ Util.manifest Util.namespace
          , Util.manifest $
              Util.deployment
                ANON
                  { initContainers =
                      [ plugins $
                          Util.container
                            (registry <> "gitbucket/plugins")
                            ANON
                              { command = ["cp", "-r", "/plugins", pluginsDir]
                              , volumeMounts =
                                  [ plugins $ Util.volumeMount pluginsDir
                                  ]
                              }
                      ]
                  , containers =
                      [ Util.container
                          (registry <> "gitbucket:latest")
                          ANON
                            { ports = [Util.containerPort 8080]
                            , -- , livenessProbe = Util.probe $ Util.httpGet "/api/v3"
                              -- , readinessProbe = Util.probe $ Util.httpGet "/api/v3"
                              volumeMounts =
                                [ Util.volumeMount "/home/nonroot/.gitbucket"
                                , plugins $ Util.volumeMount pluginsDir
                                ]
                            }
                      ]
                  , volumes =
                      [ toJSON Util.persistentVolumeClaimVolume
                      , toJSON $ plugins Util.emptyDirVolume
                      ]
                  , securityContext = ANON{fsGroup = 65532 :: Int}
                  }
          , Util.manifest $ Util.openebsLvmClaim "5Gi"
          , Util.manifest $ Util.service ANON{ports = [Util.httpServicePort]}
          , Util.manifest $ Util.ingressContourTls [Util.ingressRule "/"]
          ]

yamls :: [Yaml]
yamls =
  mconcat
    [ certManager
    , dns
    , gitbucket
    , openebs
    , projectcontour
    , registry
    ]