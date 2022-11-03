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

### harborのコンテナレジストリに接続するために

* `nsswitch.conf`のhostsのdnsをmyhostnameより前に移動(`authselect select custom/dns-first`)
* ca.certを/etc/containers/certs.d/レジストリのドメイン/ca.crtに配置<https://qiita.com/ysakashita/items/12e2f1e902ca5cbd56ed#kubernetes%E3%81%AEnode%E3%81%B8cacrt%E3%82%92%E9%85%8D%E7%BD%AE>
