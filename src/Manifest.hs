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

import qualified Data.Aeson.KeyMap as KeyMap
import Secret (externalIp, host)
import TH (embedModifedYamlFile, embedYamlAllFile, embedYamlFile)
import Util (HelmValues, Manifest)
import qualified Util

-- config for coredns
dns :: Manifest
dns =
  let ?namespace = "dns"
      ?name = "dns"
   in let mountPath = "/etc/coredns/"
       in Util.manifest
            [ toJSON Util.namespace
            , toJSON $
                Util.deployment
                  ANON
                    { containers =
                        [ Util.container
                            "coredns"
                            "registry.k8s.io/coredns/coredns:v1.9.3"
                            ANON
                              { args = ["-conf", mountPath <> "Corefile"] :: [Text]
                              , command = ["/coredns"] :: [Text]
                              , livenessProbe =
                                  ANON
                                    { httpGet = Util.httpGet 8080 "health"
                                    , initialDelaySeconds = 60 :: Scientific
                                    , timeoutSeconds = 5 :: Scientific
                                    , successThreshold = 1 :: Int
                                    , failureThreshold = 5 :: Int
                                    }
                              , ports =
                                  [ toJSON $ Util.containerPort 53 `merge` ANON{protocol = "UDP" :: Text}
                                  , toJSON $ Util.addSuffix "metrics" $ Util.containerPort 9153
                                  ]
                              , readinessProbe = ANON{httpGet = Util.httpGet 8081 "ready"}
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
            , toJSON $
                Util.configMap (KeyMap.singleton "Corefile" ($(embedStringFile "src/dns/Corefile") :: Text))
            , toJSON $
                Util.service $
                  ANON
                    { externalIPs = [externalIp]
                    , ports =
                        [ toJSON $
                            Util.servicePort 53
                              `merge` ANON{protocol = "UDP" :: Text}
                        ]
                    }
            ]

nginxIngressController :: Manifest
nginxIngressController =
  let ?namespace = "ingress-nginx"
      ?name = "ingress-nginx-controller"
   in let [service, configMap, ingressClass] =
            $(embedYamlAllFile "src/ingress-nginx/ingress-nginx-controller.yaml")
       in Util.manifest
            [ over
                (key "spec" % _Object)
                (KeyMap.insert "externalIPs" $ toJSON [externalIp])
                service
            , over
                (key "data" % _Object)
                (KeyMap.insert "proxy-body-size" $ toJSON ("50g" :: Text))
                configMap
            , over
                (key "metadata" % _Object)
                ( KeyMap.insert "annotations" $
                    toJSON $
                      KeyMap.singleton @Text "ingressclass.kubernetes.io/is-default-class" "true"
                )
                ingressClass
            ]

dockerRegistry :: Manifest
dockerRegistry =
  -- can be run by user
  let ?namespace = "docker-registry"
      ?name = "docker-registry"
   in let config = Util.addSuffix "config"
       in Util.manifest
            [ toJSON Util.namespace
            , toJSON $
                Util.deployment $
                  ANON
                    { containers =
                        [ Util.mirror Util.container "registry" $
                            ANON
                              { ports = [Util.containerPort 5000]
                              , volumeMounts =
                                  [ Util.volumeMount "/var/lib/registry"
                                  , config $ Util.volumeMount "/etc/docker/registry"
                                  ]
                              }
                        ]
                    , volumes =
                        [ toJSON Util.persistentVolumeClaimVolume
                        , toJSON $ config Util.configMapVolume
                        ]
                    }
            , toJSON $ Util.openebsLvmClaim "5Gi"
            , toJSON $
                config $
                  Util.configMap $
                    KeyMap.singleton "config.yml" ($(embedStringFile "src/registry/config.yml") :: Text)
            , toJSON $
                Util.service $
                  ANON
                    { ports = [Util.servicePort 5000]
                    }
            , toJSON $ Util.ingressNginxTls [Util.ingressRule $ "registry." <> host]
            ]

harbor :: HelmValues
harbor =
  let ?namespace = "registry"
      ?name = "harbor"
   in let url = "registry." <> host
       in Util.helmValues "harbor/harbor" $
            toJSON
              ANON
                { expose =
                    ANON
                      { ingress =
                          ANON
                            { hosts =
                                ANON
                                  { core = url
                                  , notary = "notary." <> url
                                  }
                            }
                      }
                , externalURL = url
                }

openebs :: Manifest
openebs =
  let ?name = "openebs"
   in Util.manifest [$(embedYamlFile "src/openebs/storage-class.yaml")]

manifests :: [Manifest]
manifests =
  [ dns
  , nginxIngressController
  , openebs
  ]

helmValuess :: [HelmValues]
helmValuess =
  [ harbor
  ]