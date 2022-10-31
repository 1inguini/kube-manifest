module Manifest where

import Control.Monad.State.Strict (MonadState, execState, modify)
import Data.Aeson (toJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Optics (AsJSON (_JSON), key, _Value)
import Data.FileEmbed (embedStringFile)
import qualified Data.Map as Map
import Data.Record.Anon
import Data.Record.Anon.Simple (Record, merge)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Optics (review, set, (%))

import Secret (externalIp)
import TH (embedYamlFile)
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
                            "registry.k8s.io/coredns/coredns:ioK8sApiCoreV1.9.3"
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
                configMap (Map.singleton ("Corefile" :: Text) ($(embedStringFile "src/dns/Corefile") :: Text))
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

-- ingressNginx :: App
-- ingressNginx =
--   let ?namespace = "ingress-nginx"
--       ?name = "ingress-nginx-controller"
--    in mkApp
--         { yamlsManifests =
--             [ $
--                 ( embedModifedYamlFile
--                     "src/ingress-nginx/ingress-nginx-controller.yaml"
--                     $ set (ioK8sApiCoreV1ServiceSpecL . _Just . ioK8sApiCoreV1ServiceSpecExternalIpsL)
--                     $ Just [externalIp]
--                 )
--             ]
--         }

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

-- dockerRegistry :: App
-- dockerRegistry =
--   -- can be run by user
--   let ?namespace = "docker-registry"
--       ?name = "docker-registry"
--    in let config = addSuffix "config"
--           registryPort = "registry"
--        in mkApp
--             { yamlsManifests =
--                 [ toJSON namespace,
--                   toJSON $
--                     deployment $
--                       ( mkIoK8sApiCoreV1PodSpec
--                           [ (mirror container "registry")
--                               { ioK8sApiCoreV1ContainerPorts = Just [namedPort registryPort 5000],
--                                 ioK8sApiCoreV1ContainerVolumeMounts =
--                                   Just
--                                     [ mkIoK8sApiCoreV1VolumeMount "/var/lib/registry" ?name,
--                                       config $ mkIoK8sApiCoreV1VolumeMount "/etc/docker/registry" ?name
--                                     ]
--                               }
--                           ]
--                       )
--                         { ioK8sApiCoreV1PodSpecVolumes =
--                             Just
--                               [ persistentVolumeClaimVolume,
--                                 config configMapVolume
--                               ]
--                         },
--                   toJSON $ openebsLvmClaim "1Gi",
--                   toJSON $
--                     config $
--                       configMap $
--                         Map.singleton "config.yml" $
--                           (embedStringFile "src/registry/config.yml"),
--                   toJSON $
--                     service $
--                       serviceSpec
--                         { -- ioK8sApiCoreV1ServiceSpecExternalIps = Just [externalIp],
--                           ioK8sApiCoreV1ServiceSpecPorts = Just [servicePort registryPort 5000]
--                         },
--                   toJSON $
--                     ingress
--                       [ mkIoK8sApiNetworkingV1IngressRule
--                           { ioK8sApiNetworkingV1IngressRuleHost = Just $ "registry." <> host
--                           }
--                       ]
--                 ]
--             }

-- -- gitHttp :: App
-- -- gitHttp =
-- --   let ?namespace = "default"
-- --       ?name = "git-http"
-- --    in let repos = addSuffix "repos"
-- --           cgi = addSuffix "cgi"
-- --           homeDir = "/srv/httpd/"
-- --        in mkApp
-- --             { yamlsManifests =
-- --                 [ toJSON $
-- --                     deployment $
-- --                       ( mkIoK8sApiCoreV1PodSpec
-- --                           [ (container ?name $ registry <> "git-http")
-- --                               { ioK8sApiCoreV1ContainerPorts = Just [mirror port 5001],
-- --                                 ioK8sApiCoreV1ContainerCommand = Just ["/usr/bin/busybox"],
-- --                                 ioK8sApiCoreV1ContainerArgs =
-- --                                   Just
-- --                                     [ "httpd",
-- --                                       "-f",
-- --                                       "-v",
-- --                                       "-p",
-- --                                       "5001",
-- --                                       "-h",
-- --                                       homeDir
-- --                                     ],
-- --                                 ioK8sApiCoreV1ContainerSecurityContext =
-- --                                   Just $
-- --                                     mkIoK8sApiCoreV1SecurityContext
-- --                                       { ioK8sApiCoreV1SecurityContextRunAsUser = Just 0,
-- --                                         ioK8sApiCoreV1SecurityContextPrivileged = Just True
-- --                                       },
-- --                                 ioK8sApiCoreV1ContainerVolumeMounts =
-- --                                   Just
-- --                                     [ repos $ mkIoK8sApiCoreV1VolumeMount "/srv/repos" ?name,
-- --                                       cgi $ mkIoK8sApiCoreV1VolumeMount (homeDir <> "cgi-bin") ?name
-- --                                     ]
-- --                               }
-- --                           ]
-- --                       )
-- --                         { ioK8sApiCoreV1PodSpecVolumes =
-- --                             Just
-- --                               [ repos $ hostPath "/srv/repos",
-- --                                 cgi $
-- --                                   set
-- --                                     (ioK8sApiCoreV1VolumeConfigMapL . _Just . ioK8sApiCoreV1ConfigMapVolumeSourceDefaultModeL)
-- --                                     (Just 0o755)
-- --                                     configMapVolume
-- --                               ]
-- --                         },
-- --                   cgi $
-- --                     toJSON $
-- --                       configMap $
-- --                         Map.singleton "cgi.sh" $(embedStringFile "src/git-http/cgi.sh")
-- --                 ]
-- --             }

-- openebs :: App
-- openebs =
--   let ?name = "openebs"
--    in mkApp
--         { yamlsManifests = [$ (embedYamlFile "src/openebs/storage-class.yaml")]
--         }

manifests :: [Manifest]
manifests = [dns]
