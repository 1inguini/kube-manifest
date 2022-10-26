module Util where

-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map

import Control.Monad.State.Strict (execState)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Records (HasField)

-- import Manifest.Io.K8s.Api.Core.V1 (Namespace)
-- import Manifest.Io.K8s.Apimachinery.Pkg.Apis.Meta.V1 (ObjectMeta (..))
import Optics (
  A_Prism,
  A_Setter,
  LabelOptic',
  assign,
  review,
  (%),
  _Just,
 )

mirror :: (a -> a -> b) -> a -> b
mirror f a = f a a

name :: (?name :: Text) => a -> (Text, a)
name = (,) ?name

addSuffix :: (?name :: Text) => Text -> ((?name :: Text) => a) -> a
addSuffix suffix x = let ?name = ?name <> "-" <> suffix in x

primaryLabelKey :: Text
primaryLabelKey = "app"

-- meta ::
--   (?name :: Text, ?namespace :: Text) => ObjectMeta
-- meta =
--   ObjectMeta
--     { annotations = Nothing,
--       creationTimestamp = Nothing,
--       deletionGracePeriodSeconds = Nothing,
--       deletionTimestamp = Nothing,
--       finalizers = Nothing,
--       generateName = Nothing,
--       generation = Nothing,
--       labels = Nothing,
--       managedFields = Nothing,
--       name = Just ?name,
--       namespace = Just ?namespace,
--       ownerReferences = Nothing,
--       resourceVersion = Nothing,
--       selfLink = Nothing,
--       uid = Nothing
--     }

-- meta ::
--   (?name :: Text, ?namespace :: Text) =>
--   ( -- LabelOptic' "metadata" A_Prism object (Maybe meta),
--     LabelOptic' "namespace" A_Prism meta (Maybe Text)
--   ) =>
--   meta
-- meta = review (#namespace % _Just) ?namespace

-- metaTest :: Namespace.Metadata
-- metaTest =
--   let ?name = ""
--       ?namespace = "hello"
--    in meta

-- meta :: (?name :: Text, ?namespace :: Text) => IoK8sApimachineryPkgApisMetaV1ObjectMeta
-- meta =
--   mkIoK8sApimachineryPkgApisMetaV1ObjectMeta
--     { ioK8sApimachineryPkgApisMetaV1ObjectMetaNamespace = Just ?namespace,
--       ioK8sApimachineryPkgApisMetaV1ObjectMetaName = Just ?name,
--       ioK8sApimachineryPkgApisMetaV1ObjectMetaLabels =
--         Just $
--           Map.fromList
--             [ (primaryLabelKey, ?name)
--             ]
--     }

-- namespace :: (?name :: Text, ?namespace :: Text) => IoK8sApiCoreV1Namespace
-- namespace =
--   mkIoK8sApiCoreV1Namespace
--     { ioK8sApiCoreV1NamespaceApiVersion = Just "v1",
--       ioK8sApiCoreV1NamespaceKind = Just "Namespace",
--       ioK8sApiCoreV1NamespaceMetadata = Just meta
--     }

-- deployment :: (?name :: Text, ?namespace :: Text) => IoK8sApiCoreV1PodSpec -> IoK8sApiAppsV1Deployment
-- deployment spec =
--   mkIoK8sApiAppsV1Deployment
--     { ioK8sApiAppsV1DeploymentApiVersion = Just "apps/v1",
--       ioK8sApiAppsV1DeploymentKind = Just "Deployment",
--       ioK8sApiAppsV1DeploymentMetadata = Just meta,
--       ioK8sApiAppsV1DeploymentSpec =
--         Just $
--           ( mkIoK8sApiAppsV1DeploymentSpec
--               ( mkIoK8sApimachineryPkgApisMetaV1LabelSelector
--                   { ioK8sApimachineryPkgApisMetaV1LabelSelectorMatchLabels = Just $ Map.singleton primaryLabelKey ?name
--                   }
--               )
--               $ mkIoK8sApiCoreV1PodTemplateSpec
--                 { ioK8sApiCoreV1PodTemplateSpecMetadata = Just meta,
--                   ioK8sApiCoreV1PodTemplateSpecSpec = Just spec
--                 }
--           )
--             { ioK8sApiAppsV1DeploymentSpecReplicas = Just 1
--             }
--     }

-- type PriorityClassName = Text

-- systemClusterCritical :: PriorityClassName
-- systemClusterCritical = "system-cluster-critical"

-- container :: (?name :: Text) => Text -> Text -> IoK8sApiCoreV1Container
-- container suffix image =
--   addSuffix suffix $
--     (mkIoK8sApiCoreV1Container ?name)
--       { ioK8sApiCoreV1ContainerImage = Just image,
--         ioK8sApiCoreV1ContainerSecurityContext =
--           Just $
--             mkIoK8sApiCoreV1SecurityContext
--               { ioK8sApiCoreV1SecurityContextAllowPrivilegeEscalation = Just False
--               }
--       }

-- namedVolume :: (?name :: Text) => IoK8sApiCoreV1Volume
-- namedVolume =
--   (mkIoK8sApiCoreV1Volume ?name)
--     { ioK8sApiCoreV1VolumePersistentVolumeClaim =
--         Just
--           IoK8sApiCoreV1PersistentVolumeClaimVolumeSource
--             { ioK8sApiCoreV1PersistentVolumeClaimVolumeSourceClaimName = ?name,
--               ioK8sApiCoreV1PersistentVolumeClaimVolumeSourceReadOnly = Just False
--             }
--     }

-- hostPath :: (?name :: Text) => Text -> IoK8sApiCoreV1Volume
-- hostPath path =
--   (mkIoK8sApiCoreV1Volume ?name)
--     { ioK8sApiCoreV1VolumeHostPath = Just $ mkIoK8sApiCoreV1HostPathVolumeSource path
--     }

-- configMapVolume :: (?name :: Text) => IoK8sApiCoreV1Volume
-- configMapVolume =
--   (mkIoK8sApiCoreV1Volume ?name)
--     { ioK8sApiCoreV1VolumeConfigMap =
--         Just $
--           mkIoK8sApiCoreV1ConfigMapVolumeSource
--             { ioK8sApiCoreV1ConfigMapVolumeSourceName = Just ?name
--             }
--     }

-- persistentVolumeClaimVolume :: (?name :: Text) => IoK8sApiCoreV1Volume
-- persistentVolumeClaimVolume =
--   (mkIoK8sApiCoreV1Volume ?name)
--     { ioK8sApiCoreV1VolumePersistentVolumeClaim =
--         Just $
--           mkIoK8sApiCoreV1PersistentVolumeClaimVolumeSource ?name
--     }

-- -- port :: Int -> Int -> ioK8sApiCoreV1ContainerPort
-- -- port hostPort containerPort =
-- --   (mkIoK8sApiCoreV1ContainerPort containerPort)
-- --     { ioK8sApiCoreV1ContainerPortHostPort = Just hostPort
-- --     }

-- namedPort :: Text -> Int -> IoK8sApiCoreV1ContainerPort
-- namedPort name =
--   set ioK8sApiCoreV1ContainerPortNameL (Just name) . mkIoK8sApiCoreV1ContainerPort

-- configMap :: (?name :: Text, ?namespace :: Text) => Map String Text -> IoK8sApiCoreV1ConfigMap
-- configMap configMapData =
--   mkIoK8sApiCoreV1ConfigMap
--     { ioK8sApiCoreV1ConfigMapApiVersion = Just "v1",
--       ioK8sApiCoreV1ConfigMapKind = Just "ConfigMap",
--       ioK8sApiCoreV1ConfigMapMetadata = Just meta,
--       ioK8sApiCoreV1ConfigMapData = Just configMapData,
--       ioK8sApiCoreV1ConfigMapImmutable = Just True
--     }

-- openebsLvmProvisioner :: Text
-- openebsLvmProvisioner = "openebs-lvmpv"

-- readWriteOnce :: Text
-- readWriteOnce = "ReadWriteOnce"

-- persistentVolumeClaim ::
--   (?name :: Text, ?namespace :: Text) =>
--   IoK8sApiCoreV1PersistentVolumeClaimSpec ->
--   IoK8sApiCoreV1PersistentVolumeClaim
-- persistentVolumeClaim spec =
--   mkIoK8sApiCoreV1PersistentVolumeClaim
--     { ioK8sApiCoreV1PersistentVolumeClaimApiVersion = Just "v1",
--       ioK8sApiCoreV1PersistentVolumeClaimKind = Just "PersistentVolumeClaim",
--       ioK8sApiCoreV1PersistentVolumeClaimMetadata = Just meta,
--       ioK8sApiCoreV1PersistentVolumeClaimSpec = Just spec
--     }

-- openebsLvmClaim ::
--   (?name :: Text, ?namespace :: Text) =>
--   Text ->
--   IoK8sApiCoreV1PersistentVolumeClaim
-- openebsLvmClaim size =
--   persistentVolumeClaim $
--     mkIoK8sApiCoreV1PersistentVolumeClaimSpec
--       { ioK8sApiCoreV1PersistentVolumeClaimSpecStorageClassName = Just openebsLvmProvisioner,
--         ioK8sApiCoreV1PersistentVolumeClaimSpecAccessModes = Just [readWriteOnce],
--         ioK8sApiCoreV1PersistentVolumeClaimSpecResources =
--           Just $
--             mkIoK8sApiCoreV1ResourceRequirements
--               { ioK8sApiCoreV1ResourceRequirementsRequests = Just $ Map.singleton "storage" size
--               }
--       }

-- service :: (?name :: Text, ?namespace :: Text) => IoK8sApiCoreV1ServiceSpec -> IoK8sApiCoreV1Service
-- service spec =
--   mkIoK8sApiCoreV1Service
--     { ioK8sApiCoreV1ServiceApiVersion = Just "v1",
--       ioK8sApiCoreV1ServiceKind = Just "Service",
--       ioK8sApiCoreV1ServiceMetadata = Just meta,
--       ioK8sApiCoreV1ServiceSpec = Just spec
--     }

-- serviceSpec :: (?name :: Text) => IoK8sApiCoreV1ServiceSpec
-- serviceSpec =
--   mkIoK8sApiCoreV1ServiceSpec
--     { ioK8sApiCoreV1ServiceSpecSelector = Just $ Map.singleton primaryLabelKey ?name
--     }

-- servicePort :: Text -> Int -> IoK8sApiCoreV1ServicePort
-- servicePort name = set ioK8sApiCoreV1ServicePortTargetPortL (Just name) . mkIoK8sApiCoreV1ServicePort

-- ingress :: (?name :: Text, ?namespace :: Text) => [IoK8sApiNetworkingV1IngressRule] -> IoK8sApiNetworkingV1Ingress
-- ingress rules =
--   mkIoK8sApiNetworkingV1Ingress
--     { ioK8sApiNetworkingV1IngressApiVersion = Just "networking.k8s.io/v1",
--       ioK8sApiNetworkingV1IngressKind = Just "Ingress",
--       ioK8sApiNetworkingV1IngressMetadata = Just meta,
--       ioK8sApiNetworkingV1IngressSpec =
--         Just $
--           mkIoK8sApiNetworkingV1IngressSpec
--             { ioK8sApiNetworkingV1IngressSpecIngressClassName = Just "nginx",
--               ioK8sApiNetworkingV1IngressSpecRules = Just rules
--             }
--     }

-- data App = App
--   { yamlsPath :: FilePath,
--     yamlsManifests :: ![Yaml.Value]
--   }
--   deriving (Show, TH.Lift)

-- mkApp :: (?name :: Text) => App
-- mkApp =
--   App
--     { yamlsPath = "./manifests/" <> Text.unpack ?name <> ".yaml",
--       yamlsManifests = []
--     }
