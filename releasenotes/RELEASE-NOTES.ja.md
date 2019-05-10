# Riak 2.1.1 リリースノート
*注: Riak 2.1.0 は Riak 2.1.1 に置き換わりました*

## 修正
Riak 2.1.0 で混入した handoff.ip のデフォルト設定に起因するバグが、 Riak 2.1.1 で修正されました。これはhandoffにおいて、転送先ノードへのデータ転送を伴わずに、データ転送済みvnodeを削除するマークが付与される問題でした。2.1.0 のユーザーがこの問題を回避するには設定変更(riak.conf)が必須となります。全ユーザーがこの問題の影響を受けるわけではありませんが、すべての 2.1.0 ユーザーに対し、2.1.1へのアップグレードを推奨します。

この問題の詳細はBasho Documentaion の [Product Advisories](http://docs.basho.com/riak/latest/community/product-advisories/210-dataloss/) で確認できます。

*  vars.config内で `handoff_ip` のデフォルト値を 0.0.0.0 へ変更。
  * [riak/pull/734](https://github.com/basho/riak/pull/734)

# Riak 2.1.0 リリースノート

## 新機能

### ライトワンス バケットタイプ

Riak 2.1.0 はライトワンス バケットのコンセプトを導入します。このバケットのエントリーは
厳密に一度しか書き込まれず、更新も上書きもされない想定です。オブジェクトへは一度の書き込みしか想定されないため、
Riak は通常の get、merge、updateというサイクルをおこないません。これはIOPsの低減や、スループットとレイテンシーの向上
をもたらします。

単一キーに対する複数エントリーの書込みは想定されませんが、可能ではあります -- APIの誤使用、ネットワークパーティション、
同時書込みなどで起こり得ます。これらのケースで Riak は、個々のオブジェクトの SHA-1 ハッシュを基にした
アルゴリズムで Sibling の解決をおこないます。このアルゴリズムはデータベースレベルで
繰返し可能かつ決定論的ですが、ユーザーにとっては、"random write wins" に見えるかもしれません。

`write_once` プロパティはバケットタイプへ適用されるBoolean値であり、バケットタイプ作成時にのみ設定できます。
バケットタイプが設定され、`write_once`プロバティと共にアクティベートされるとプロパティは変更できません。

制限事項:

 * Pre/Post-commitフックはサポートされません
 * ラージオブジェクトの警告や制限がはたらきません
 * `write_once` を有効化する前に全ノードをアップグレードする必要があります。 バージョンが混在したまま利用するとvnodeがクラッシュします。

## 変更

* [Issue kv679](https://github.com/basho/riak_kv/issues/679) - 因果履歴の再利用によるデータロストの主要因を修正。モノトニックカウンターはvnode起動時にはディスクへ同期されるため、vnodeの起動・再起動は以前はなかったコストを持つようになる。
  * [riak_kv/pull/1070](https://github.com/basho/riak_kv/pull/1070)


* 特定のバケット・キーに対するアクティブなプリファレンスリストを取得するAPIの追加。
特定のバケット・キーをホストするノードを示すプリファレンスリスト(プライマリ、フォールバックが明示された)を取得するGET操作 `/types/Type/buckets/Bucket/keys/Key/preflist` の追加。
  * [riak-erlang-http-client/pull/50](https://github.com/basho/riak-erlang-http-client/pull/50)
  * [riak_core/pull/705](https://github.com/basho/riak_core/pull/705)
  * [riak_pb/pull/105](https://github.com/basho/riak_pb/pull/105)
  * [riak-erlang-client/pull/204](https://github.com/basho/riak-erlang-client/pull/204)
  * [riak_kv/pull/1083](https://github.com/basho/riak_kv/pull/1083)
  * [riak_api/pull/81](https://github.com/basho/riak_api/pull/81)
  * [riak_api/pull/75](https://github.com/basho/riak_api/pull/75)

* riak-admin のいくつかのコマンドに対して JSON フォーマット出力を追加。これはcluster status, cluster partition[s], cluster partition-countと riak-admin handoff、riak-admin set/show/describe コマンドの一部を含む。
  * [clique/pull/47](https://github.com/basho/clique/pull/47)

* Riakで使われる Erlang R16 へのパフォーマンス関連パッチの統合
  * [opt/pull/10](https://github.com/basho/otp/pull/10)

* riak_core_ring_manager:is_stable_ring/0 をパブリックAPIに追加。直近 x 秒以内(現在 90秒)でringの変更があったかどうかがクライアントから確認できるようになる。
  * [riak_core/pull/716](https://github.com/basho/riak_core/pull/716)

* [yokozuna/issues/468](https://github.com/basho/yokozuna/issues/468) - メトリクスへ `search_query_latency_mean` と `search_index_latency_mean` を追加。
  * [yokozuna/pull/473](https://github.com/basho/yokozuna/pull/473)

## 修正

* [yokozuna/issues/452](https://github.com/basho/yokozuna/issues/452):  Strong ConsistencyやCRDT使用時にSiblingを削除しない
  * [yokozuna/pull/457](https://github.com/basho/yokozuna/pull/457)

* [yokozuna/issues/402](https://github.com/basho/yokozuna/issues/402): Indexが有効化される前にHTTPのレスポンスを返す。
  * [riak_test/pull/745](https://github.com/basho/riak_test/pull/745)
  * [riak_pb/pull/112](https://github.com/basho/riak_pb/pull/112)
  * [riak_pb/pull/111](https://github.com/basho/riak_pb/pull/111)
  * [riak-erlang-client/pull/207](https://github.com/basho/riak-erlang-client/pull/207)
  * [yokozuna/pull/463](https://github.com/basho/yokozuna/pull/463)

* [yokozuna/issues/450](https://github.com/basho/yokozuna/issues/450): AAEが毎時 {badarg,46}, [{base64,decode_binary[{base64.erl… の終了値で失敗する
  * [yokozuna/pull/459](https://github.com/basho/yokozuna/pull/459)

* [yokozuna/issues/437](https://github.com/basho/yokozuna/issues/437): yz_events:handle_info が誤った引数で呼ばれる
  * [yokozuna/pull/458](https://github.com/basho/yokozuna/pull/458)
  * [yokozuna/pull/42](https://github.com/basho/yokozuna/pull/42)

* [yokozuna/issues/469](https://github.com/basho/yokozuna/issues/469): YZの統計値のtypo修正
  * [yokozuna/pull/470](https://github.com/basho/yokozuna/pull/470)

* [riak_core/issues/698](https://github.com/basho/riak_core/issues/698): ワイルドカード名にマッチするピアからの SSL コネクションが常に拒否される
  * [riak_core/pull/718](https://github.com/basho/riak_core/pull/718) 

## 既知の問題

* [yokozuna/issues/481](https://github.com/basho/yokozuna/issues/481) - YZ AAEツリーが期限切れの際にYokozunaがエントリーを失う。現在調査中。
* [riak/issues/727](https://github.com/basho/riak/issues/727) - 1.4.x から 2.1.x へアップグレードするユーザーが `app.config` を利用する場合、`allow_mult` と `dvv_enabled` のデフォルト設定が `false` から `true` へ変更されることに注意。これらの設定が `false` であることにアプリケーションが依存している場合、後方互換を維持するために、明示的な設定 `{default_bucket_props, [{allow_mult, false}, {dvv_enabled, false}]},` を `app.config` の `riak_core` セクションへ設定する必要がある。

