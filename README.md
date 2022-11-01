cni0インターフェースはtrustedにブチ込んでおく

## Harbor

``` bash
helm repo add harbor https://helm.goharbor.io
helm template --values value/harbor.yaml harbor/harbor > manifest/harbor.yaml
```
