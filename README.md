# kube-manifest

cni0インターフェースはtrustedにブチ込んでおく

`sudo ln -s /run/crio/crio.sock /run/containerd/containerd.sock`で無理やり[BuildKit CLI for kubectl](https://github.com/vmware-tanzu/buildkit-cli-for-kubectl#buildkit-cli-for-kubectl)を動かす → だめでした

諦めてcri-oからcontainerdに

結局`kubectl build`をやめてcri-oと`nerdctl build`に

## コンテナ

## builder container

files.shにarch linuxのパッケージ(aurでも可)を渡すと依存するファイルだけを/tmpにコピーするので、
マルチステージビルドで`gcr.io/distroless/static`にでも`COPY`しましょう

``` bash
podman build containers/php-fpm --rm --tag registry.1inguini.com/library/php-fpm:$(date --utc +%Y%m%d)
```

## mariadb

rootの初期パスワードは`secret`

/var/lib/mysqlは空なのでregistry.1inguini.com/library/mariadb/varを無限ループでもさせて`podman run --volumes-from ~~`

## マニフェスト

``` bash
cabal run kube-manifest
```

でmanifest/namespace/app.yamlが生成される

### Harbor

``` bash
helm repo add harbor https://helm.goharbor.io
helm template --values values/harbor.yaml harbor/harbor > manifest/harbor.yaml
```

#### harborのコンテナレジストリに接続するために

* `nsswitch.conf`のhostsのdnsをmyhostnameより前に移動(`authselect select custom/dns-first`)
* `kubectl get secrets -n registry harbor -o yaml`のca.crtをbase64デコードして
  Fedoraなら/usr/share/pki/ca-trust-source/anchors/なんか適当な名前.pem
  (/etc/containers/certs.d/レジストリのドメイン/ca.crtでもおそらく大丈夫
  <https://qiita.com/ysakashita/items/12e2f1e902ca5cbd56ed#kubernetes%E3%81%AEnode%E3%81%B8cacrt%E3%82%92%E9%85%8D%E7%BD%AE>)
  に置いて`update-ca-trust`

## TODO

* gitbucketにmariadb
* `embedYamlFromDirectory`、Kubernetesオブジェクトの`Aeson.Value`のリストを`Ix`に

## Application

[x] Harbor
[ ] GitBucket
[ ] roundcube
[ ] snikket
[ ] roundcube
[ ] mail server
