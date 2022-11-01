cni0インターフェースはtrustedにブチ込んでおく

## Harbor

``` bash
helm repo add harbor https://helm.goharbor.io
helm template --values values/harbor.yaml harbor/harbor > manifest/harbor.yaml
```
