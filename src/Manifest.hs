module Manifest where

import Control.Monad.State.Strict (MonadState, execState, modify)
import Data.Aeson (toJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Optics (AsJSON (_JSON), key, _Value)
import Data.Record.Anon
import Data.Record.Anon.Simple (Record)
import Data.Text (Text)
import qualified Data.Text as Text
import Optics (review, set, (%))

import Secret (externalIp)
import Util

-- -- config for coredns
-- dns :: App
-- dns =
--   let ?namespace = "dns"
--       ?name = "dns"
--    in let mountPath = "/etc/coredns/"
--           dns = "dns"
--        in mkApp
--             { yamlsManifests =
--                 [ toJSON namespace,
--                   toJSON $
--                     deployment $
--                       ( mkIoK8sApiCoreV1PodSpec
--                           [ over
--                               (ioK8sApiCoreV1ContainerSecurityContextL . _Just)
--                               ( set
--                                   ioK8sApiCoreV1SecurityContextCapabilitiesL
--                                   ( Just $
--                                       mkIoK8sApiCoreV1Capabilities
--                                         { ioK8sApiCoreV1CapabilitiesAdd = Just ["NET_BIND_SERVICE"],
--                                           ioK8sApiCoreV1CapabilitiesDrop = Just ["all"]
--                                         }
--                                   )
--                                   . set ioK8sApiCoreV1SecurityContextReadOnlyRootFilesystemL (Just True)
--                               )
--                               $ (container "coredns" "registry.k8s.io/coredns/coredns:ioK8sApiCoreV1.9.3")
--                                 { ioK8sApiCoreV1ContainerCommand = Just ["/coredns"],
--                                   ioK8sApiCoreV1ContainerArgs =
--                                     Just
--                                       [ "-conf",
--                                         mountPath <> "Corefile"
--                                       ],
--                                   ioK8sApiCoreV1ContainerVolumeMounts =
--                                     Just [mkIoK8sApiCoreV1VolumeMount mountPath ?name],
--                                   ioK8sApiCoreV1ContainerPorts =
--                                     Just
--                                       [ (namedPort dns 53)
--                                           { ioK8sApiCoreV1ContainerPortProtocol = Just E'Protocol'UDP
--                                           },
--                                         namedPort "metrics" 9153
--                                       ],
--                                   ioK8sApiCoreV1ContainerLivenessProbe =
--                                     Just $
--                                       mkIoK8sApiCoreV1Probe
--                                         { ioK8sApiCoreV1ProbeHttpGet =
--                                             Just
--                                               (mkIoK8sApiCoreV1HTTPGetAction "8080")
--                                                 { ioK8sApiCoreV1HTTPGetActionPath = Just "health"
--                                                 },
--                                           ioK8sApiCoreV1ProbeInitialDelaySeconds = Just 60,
--                                           ioK8sApiCoreV1ProbeTimeoutSeconds = Just 5,
--                                           ioK8sApiCoreV1ProbeSuccessThreshold = Just 1,
--                                           ioK8sApiCoreV1ProbeFailureThreshold = Just 5
--                                         },
--                                   ioK8sApiCoreV1ContainerReadinessProbe =
--                                     Just $
--                                       mkIoK8sApiCoreV1Probe
--                                         { ioK8sApiCoreV1ProbeHttpGet =
--                                             Just
--                                               (mkIoK8sApiCoreV1HTTPGetAction "8081")
--                                                 { ioK8sApiCoreV1HTTPGetActionPath = Just "ready"
--                                                 }
--                                         }
--                                 }
--                           ]
--                       )
--                         { ioK8sApiCoreV1PodSpecPriorityClassName = Just systemClusterCritical,
--                           ioK8sApiCoreV1PodSpecVolumes = Just [configMapVolume]
--                         },
--                   toJSON $
--                     configMap (Map.singleton "Corefile" $ (embedStringFile "src/dns/Corefile")),
--                   toJSON $
--                     service $
--                       serviceSpec
--                         { ioK8sApiCoreV1ServiceSpecExternalIps = Just [externalIp],
--                           ioK8sApiCoreV1ServiceSpecPorts =
--                             Just
--                               [ (servicePort dns 53)
--                                   { ioK8sApiCoreV1ServicePortProtocol = Just E'Protocol'UDP
--                                   }
--                               ]
--                         }
--                 ]
--             }

configMapTest :: Aeson.Value
configMapTest =
  let ?namespace = "dns"
      ?name = "dns"
   in toJSON $ configMap ANON{externalIps = externalIp}

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

-- apps :: [App]
-- apps = [dns, ingressNginx, openebs, dockerRegistry]
