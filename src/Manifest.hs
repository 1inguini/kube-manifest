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
import Optics (over, review, set, (%))

import Secret (externalIp, host)
import TH (embedModifedYamlFile, embedYamlFile)
import Util

-- config for coredns
dns :: Manifest
dns =
  let ?namespace = "dns"
      ?name = "dns"
   in let mountPath = "/etc/coredns/"
          dns = "dns"
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
                                  [ toJSON $ namedContainerPort dns 53 `merge` ANON{protocol = "UDP" :: Text}
                                  , toJSON $ namedContainerPort "metrics" 9153
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
                              , volumeMounts = [merge named ANON{mountPath = mountPath}]
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
                            namedServicePort dns 53
                              `merge` ANON{protocol = "UDP" :: Text}
                        ]
                    }
            ]

ingressNginx :: Manifest
ingressNginx =
  let ?namespace = "ingress-nginx"
      ?name = "ingress-nginx-controller"
   in manifest
        [ over
            (key "spec" % _Object)
            (KeyMap.insert "externalIPs" $ toJSON [externalIp])
            $(embedYamlFile "src/ingress-nginx/ingress-nginx-controller.yaml")
        ]

-- -- reverseProxy :: App
-- -- reverseProxy =
-- --   let ?namespace = defaultNamespace
-- --       ?name = "reverse-proxy"
-- --    in let mountPath = "/etc/caddy/"
-- --        in mkApp
-- --             { yamlsManifests =
-- --                 [ toJSON $
-- --                     deployment $
-- --                       ( mkIoK8sApiCoreV1PodSpec
-- --                           [ (container "caddy" $ registry <> "caddy")
-- --                               { ioK8sApiCoreV1ContainerPorts = Just [port 80 8000, port 443 4430],
-- --                                 ioK8sApiCoreV1ContainerCommand = Just ["/usr/bin/caddy"],
-- --                                 ioK8sApiCoreV1ContainerArgs =
-- --                                   Just
-- --                                     [ "run",
-- --                                       "--config",
-- --                                       mountPath <> "Caddyfile"
-- --                                     ],
-- --                                 ioK8sApiCoreV1ContainerVolumeMounts =
-- --                                   Just [mkIoK8sApiCoreV1VolumeMount mountPath ?name]
-- --                               }
-- --                           ]
-- --                       )
-- --                         { ioK8sApiCoreV1PodSpecVolumes = Just [configMapVolume]
-- --                         },
-- --                   toJSON $
-- --                     configMap $
-- --                       Map.singleton "Caddyfile" $(embedStringFile "src/reverse-proxy/Caddyfile")
-- --                 ]
-- --             }

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
                              { ports = [namedContainerPort ?name 5000]
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
            , toJSON $ openebsLvmClaim "1Gi"
            , toJSON $
                config $
                  configMap $
                    KeyMap.singleton "config.yml" ($(embedStringFile "src/registry/config.yml") :: Text)
            , toJSON $
                service $
                  ANON
                    { ports = [namedServicePort ?name 5000]
                    , externalIPs = [externalIp]
                    }
            , toJSON $
                ingress
                  [ ANON
                      { host = "registry." <> host
                      , http =
                          ANON
                            { paths =
                                [ ANON
                                    { backend = ANON{service = ANON{name = ?name, port = named}}
                                    , path = "/" :: Text
                                    , pathType = "Prefix" :: Text
                                    }
                                ]
                            }
                      }
                  ]
            ]

openebs :: Manifest
openebs =
  let ?name = "openebs"
   in manifest [$(embedYamlFile "src/openebs/storage-class.yaml")]

manifests :: [Manifest]
manifests =
  [ dns
  , dockerRegistry
  , ingressNginx
  , openebs
  ]
