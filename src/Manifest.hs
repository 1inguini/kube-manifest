module Manifest where

import Control.Monad.State.Strict (MonadState, execState, modify)
import Data.Aeson (toJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (AsJSON (_JSON), key, _Object)
import Data.FileEmbed (embedStringFile)
import Data.Record.Anon
import Data.Record.Anon.Simple (Record, inject, merge, project)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Optics (ix, over, review, set, (%))

import Secret (externalIp, host)
import TH (embedModifedYamlFile, embedYamlAllFile, embedYamlFile)
import Util

-- config for coredns
dns :: Manifest
dns =
  let ?namespace = "dns"
      ?name = "dns"
   in let mountPath = "/etc/coredns/"
       in manifest
            [ toJSON namespace
            , toJSON $
                deployment
                  ANON
                    { containers =
                        [ container
                            "coredns"
                            "registry.k8s.io/coredns/coredns:v1.9.3"
                            ANON
                              { args = ["-conf", mountPath <> "Corefile"] :: [Text]
                              , command = ["/coredns"] :: [Text]
                              , livenessProbe =
                                  ANON
                                    { httpGet = httpGet 8080 "health"
                                    , initialDelaySeconds = 60 :: Scientific
                                    , timeoutSeconds = 5 :: Scientific
                                    , successThreshold = 1 :: Int
                                    , failureThreshold = 5 :: Int
                                    }
                              , ports =
                                  [ toJSON $ containerPort 53 `merge` ANON{protocol = "UDP" :: Text}
                                  , toJSON $ addSuffix "metrics" $ containerPort 9153
                                  ]
                              , readinessProbe = ANON{httpGet = httpGet 8081 "ready"}
                              , securityContext =
                                  ANON
                                    { capabilities =
                                        ANON
                                          { add = ["NET_BIND_SERVICE"] :: [Text]
                                          , drop = ["all"] :: [Text]
                                          }
                                    , readOnlyRootFilesystem = True
                                    }
                              , volumeMounts = [volumeMount mountPath]
                              }
                        ]
                    , priorityClassName = systemClusterCritical
                    , volumes = [configMapVolume]
                    }
            , toJSON $
                configMap (KeyMap.singleton "Corefile" ($(embedStringFile "src/dns/Corefile") :: Text))
            , toJSON $
                service $
                  ANON
                    { externalIPs = [externalIp]
                    , ports =
                        [ toJSON $
                            servicePort 53
                              `merge` ANON{protocol = "UDP" :: Text}
                        ]
                    }
            ]

nginxIngressController :: Manifest
nginxIngressController =
  let ?namespace = "ingress-nginx"
      ?name = "ingress-nginx-controller"
   in let [service, configMap] = $(embedYamlAllFile "src/ingress-nginx/ingress-nginx-controller.yaml")
       in manifest
            [ over
                (key "spec" % _Object)
                (KeyMap.insert "externalIPs" $ toJSON [externalIp])
                service
            , over
                (key "data" % _Object)
                (KeyMap.insert "proxy-body-size" $ toJSON ("50g" :: Text))
                configMap
            ]

dockerRegistry :: Manifest
dockerRegistry =
  -- can be run by user
  let ?namespace = "docker-registry"
      ?name = "docker-registry"
   in let config = addSuffix "config"
       in manifest
            [ toJSON namespace
            , toJSON $
                deployment $
                  ANON
                    { containers =
                        [ mirror container "registry" $
                            ANON
                              { ports = [containerPort 5000]
                              , volumeMounts =
                                  [ volumeMount "/var/lib/registry"
                                  , config $ volumeMount "/etc/docker/registry"
                                  ]
                              }
                        ]
                    , volumes =
                        [ toJSON persistentVolumeClaimVolume
                        , toJSON $ config configMapVolume
                        ]
                    }
            , toJSON $ openebsLvmClaim "5Gi"
            , toJSON $
                config $
                  configMap $
                    KeyMap.singleton "config.yml" ($(embedStringFile "src/registry/config.yml") :: Text)
            , toJSON $
                service $
                  ANON
                    { ports = [servicePort 5000]
                    }
            , toJSON $ ingressNginxTls [ingressRule $ "registry." <> host]
            ]

openebs :: Manifest
openebs =
  let ?name = "openebs"
   in manifest [$(embedYamlFile "src/openebs/storage-class.yaml")]

manifests :: [Manifest]
manifests =
  [ dns
  , dockerRegistry
  , nginxIngressController
  , openebs
  ]
