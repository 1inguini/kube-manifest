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
      ?name = "openebs"
   in [Util.manifest $(embedYamlFile "src/openebs/storage-class.yaml")]

certManager :: [Yaml]
certManager =
  let ?namespace = Util.noNamespace
      ?name = "cert-manager"
   in [Util.manifest $(embedYamlFile "src/cert-manager/cluster-issuer.yaml")]

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

projectcontour :: [Yaml]
projectcontour =
  let ?namespace = "projectcontour"
      ?name = "contour"
   in [ Util.manifest $
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
      ?name = "harbor"
   in [ Util.helmValues
          "harbor/harbor"
          ANON
            { expose =
                -- Anon.insert #type ("ignore" :: Text) $ -- "ignore" is invalid value, so no ingress will be made
                ANON
                  { ingress =
                      ANON
                        { annotations =
                            KeyMap.singleton
                              "cert-manager.io/cluster-issuer"
                              Util.clusterIssuer
                        , hosts =
                            ANON
                              { core = Util.domain
                              , notary = "notary." <> Util.domain
                              }
                        }
                  , tls = ANON{auto = ANON{commonName = Util.domain}}
                  }
            , externalURL = "https://" <> Util.domain
            }
      ]

gitbucket :: [Yaml]
gitbucket =
  let ?namespace = "git" :: Text
      ?name = "gitbucket" :: Text
   in [Util.manifest Util.namespace]

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