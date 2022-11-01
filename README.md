# kube-manifest

cni0インターフェースはtrustedにブチ込んでおく

## builder container

files.shにarch linuxのパッケージ(aurでも可)を渡すと依存するファイルだけを/tmpにコピーするので、
マルチステージビルドで`gcr.io/distroless/static`にでも`COPY`しましょう

``` bash
podman build containers/php-fpm --rm --tag registry.1inguini.com/library/php-fpm:$(date --utc +%Y%m%d)
```

## Harbor

``` bash
helm repo add harbor https://helm.goharbor.io
helm template --values values/harbor.yaml harbor/harbor > manifest/harbor.yaml
```
