module Manifest where

import Control.Monad.State.Strict (MonadState, execState, modify)
import Data.Aeson (toJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Optics (AsJSON (_JSON), key, _Object)
import Data.FileEmbed (embedStringFile)
import Data.Record.Anon
import Data.Record.Anon.Simple (Record, inject, merge, project)
import Data.Record.Anon.Simple as Anon
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Optics (ix, modifying, over, review, set, (%))

import qualified Data.Aeson.KeyMap as KeyMap
import Secret (externalIp, host)
import TH (embedModifedYamlFile, embedYamlAllFile, embedYamlFile)
import Util (Yaml, YamlType (..))
import qualified Util

-- config for coredns
dns :: [Yaml]
dns =
  let ?namespace = "dns"
      ?name = "dns"
   in let mountPath = "/etc/coredns/"
       in [ Util.manifest Util.namespace
          , Util.manifest $
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
          ]

certManager :: [Yaml]
certManager =
  let ?name = "selfsigned-cluster-issuer"
   in [Util.manifestNoNamespace $(embedYamlFile "src/cert-manager/cluster-issuer.yaml")]

contour :: [Yaml]
contour =
  let ?namespace = "projectcontour"
      ?name = "contour"
   in [ Util.manifest
          $(embedYamlFile "src/projectcontour/gateway-class.yaml")
      , Util.manifest $
          let ?name = "envoy"
           in Util.service
                ANON
                  { externalIPs = [externalIp]
                  , ports =
                      [ let ?name = "http" in Util.servicePort 80
                      , let ?name = "https" in Util.servicePort 443
                      ]
                  , selector = ANON{app = ?name}
                  }
      , Util.manifest $
          Util.gateway
            ANON
              { listeners =
                  [ toJSON
                      ANON
                        { name = "http" :: Text
                        , port = 80 :: Int
                        , protocol = "HTTP" :: Text
                        , allowedRoutes = ANON{namespaces = ANON{from = "All" :: Text}}
                        }
                        -- , toJSON
                        --     ANON
                        --       { name = "https" :: Text
                        --       , hostname = "*." <> host
                        --       , port = 443 :: Int
                        --       , protocol = "HTTPS" :: Text
                        --       , allowedRoutes = ANON{namespaces = ANON{from = "All" :: Text}}
                        --       , tls =
                        --           ANON
                        --             { mode = "Terminate" :: Text
                        --             , certificateRefs = [Util.named]
                        --             }
                        --       }
                  ]
              }
      ]

nginxIngressController :: [Yaml]
nginxIngressController =
  let ?namespace = "ingress-nginx"
      ?name = "ingress-nginx-controller"
   in let [service, configMap, ingressClass] =
            $(embedYamlAllFile "src/ingress-nginx/ingress-nginx-controller.yaml")
       in Util.manifest
            <$> [ over
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

dockerRegistry :: [Yaml]
dockerRegistry =
  -- can be run by user
  let ?namespace = "docker-registry"
      ?name = "docker-registry"
   in let config = Util.addSuffix "config"
       in [ Util.manifest Util.namespace
          , Util.manifest $
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
          , Util.manifest $ Util.openebsLvmClaim "5Gi"
          , Util.manifest $
              config $
                Util.configMap $
                  KeyMap.singleton "config.yml" ($(embedStringFile "src/registry/config.yml") :: Text)
          , Util.manifest $
              Util.service $
                ANON
                  { ports = [Util.servicePort 5000]
                  }
          , Util.manifest $ Util.ingressNginxTls [Util.ingressRule $ "registry." <> host]
          ]

harbor :: [Yaml]
harbor =
  let ?namespace = "repositry" :: Text
      ?name = "harbor" :: Text
   in let domain = ?namespace <> "." <> host
       in Util.helmValues
            "harbor/harbor"
            ( toJSON
                ANON
                  { expose =
                      Anon.insert #type ("ignore" :: Text) $ -- "ignore" is invalid value, so no ingress will be made
                        ANON
                          { ingress =
                              ANON
                                { hosts =
                                    ANON
                                      { core = domain
                                      , notary = "notary." <> domain
                                      }
                                }
                          , tls = ANON{auto = ANON{commonName = domain}}
                          }
                  , externalURL = "https://" <> domain
                  }
            )
            : []

openebs :: [Yaml]
openebs =
  let ?name = "openebs"
   in [Util.manifestNoNamespace $(embedYamlFile "src/openebs/storage-class.yaml")]

yamls :: [Yaml]
yamls =
  mconcat
    [ certManager
    , contour
    , dns
    , nginxIngressController
    , openebs
    , harbor
    ]