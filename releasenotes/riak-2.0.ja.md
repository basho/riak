# Riak 2.0.1 リリースノート

## クライアント証明書による認証

先日リリースされた2.0以降、Riakでは認証と認可の機能が利用できるようになりました。

2.0.0では誤ったクライアント証明書が受け付けられていましたが、
この問題は2.0.1で修正されました。

#### マージ済PR

* bitcask/186: [Bugfix/key transform crash](https://github.com/basho/bitcask/pull/186)
* bitcask/189: [Refresh efile port if gone](https://github.com/basho/bitcask/pull/189)
* bitcask/190: [Fix scan error deadlock](https://github.com/basho/bitcask/pull/190)
* bitcask/192: [Fix remove expired on read race](https://github.com/basho/bitcask/pull/192)
* bitcask/197: [Fix extra tombstones on update](https://github.com/basho/bitcask/pull/197)
* bitcask/198: [Fix race listing readable files](https://github.com/basho/bitcask/pull/198)
* riak_kv/1008: [Use SC bucket types and buckets to know ensembles](https://github.com/basho/riak_kv/pull/1008)
* riak_kv/1026: [Update to use new breadth-first AAE exchange](https://github.com/basho/riak_kv/pull/1026)
* riak_core/626: [Allow handoff sender to abort handoff by throw'ing from fold fun](https://github.com/basho/riak_core/pull/626)
* riak_core/627: [Handoff sender sends sync periodically](https://github.com/basho/riak_core/pull/627)
* riak_core/629: [Add breadth-first AAE exchange](https://github.com/basho/riak_core/pull/629)
* riak_api/66: [Do not treat errors as success](https://github.com/basho/riak_api/pull/66)
* riak_repl/618: [Added a worker pool for fullsync sinks.](https://github.com/basho/riak_repl/pull/618)
* riak_repl/619: [Small user experience fixes.](https://github.com/basho/riak_repl/pull/619)
* riak_repl/620: [Improved AAE fullsync integration/2.0 pull request](https://github.com/basho/riak_repl/pull/620)

# Riak 2.0.0 リリースノート

## 2.0の主な機能と改善

バージョン 2.0 での新機能の一覧と説明は
[公式ドキュメント](http://docs.basho.com/riak/2.0.0/intro-v20/)
をご覧ください。関連するドキュメントへのリンクも含まれています。
また、[Upgrading to 2.0 Guide](http://docs.basho.com/riak/2.0.0/upgrade-v20/)
もあります。以下は、その技術的な詳細になります。

### バケットタイプ

Riakの以前のバージョンではキーを論理的にグループ化する仕組みとしてバケッ
トを利用し、特定データの種類とその設定を関連付けていました。Riak 2.0で
はバケットタイプの追加により、設定とバケットのグループを関連付けます。
それにより、これはもうひとつの名前空間として振る舞います。

バケットタイプは、その情報をクラスタ内に適切に行き渡らせるため、使用前
に明示的な作成と有効化が必要であります。この点はバケットと異なります。
また `consistent` および `datatype` プロパティは作成後の変更ができない
ので注意して下さい。これらはそれぞれ Strong Consistency と Riak データ
型に関わる設定で、以下に説明があります。その他のプロパティは変更が可能
です。バケットタイプでグループ化されたバケットはバケットタイプの全プロ
パティを継承します。各バケットはプロパティを個別に上書きできますが、上
書きできないプロパティもいくつか存在します。

バケットタイプは `riak-admin bucket-type` コマンドだけから管理できます。
このコマンドの形式は今後のパッチリリースで変更される可能性があります。
本リリースではコマンドの操作に対応する API は含まれません。しかし、
Bucket Properties HTTP API および Protocol Buffers メッセージ、そして
サポートされたクライアントにはバケットタイプ配下のバケットに対する
Bucket Propertiesの設定、参照ができるよう更新されています。

バケットタイプに関する詳細は
[公式ドキュメント](http://docs.basho.com/riak/2.0.0/dev/advanced/bucket-types/)
をご覧ください。

### 収束データ型（Convergent Data Types）

Riak 1.4 では結果整合カウンター（eventually consistent conunter）が
Riakへ追加されました。バージョン 2.0 ではこの成果を踏まえ、更なる収束デー
タ型(Riak データ型と呼びます)を提供します。これらのデータ型はCRDT[1]で
あり、豊富でさらに増加しつつある理論的な研究を基にしています。これまで
の Riak は保存された値を不透明なものとしてだけ扱っていましたが、データ
型により、その振る舞いから脱却します。Riakはこれらのデータ型について
「知って」いるのです。具体的には、レプリカの衝突に際して、それを収束さ
せる規則を知っています。

データ型を使うには、まず `counter`、`set`、`map` のいずれかが
`datatype` プロパティに設定されたバケットタイプが必要です。そして、その
バケットタイプに紐付くバケットへ保存する必要があります。注意点として、
そのバケットは `allow_mult` プロパティとして `true` を持つことが必要で
す。詳細は
[データ型](http://docs.basho.com/riak/2.0.0/dev/using/data-types/)と
[バケットタイプ](http://docs.basho.com/riak/2.0.0/dev/advanced/bucket-types/)
のドキュメントをご覧ください。

これらのデータ型は通常の`riak_object`に含まれ、
Riakの値に関するサイズ制限はデータ型に対しても適用されます。
以下のデータ型が現状で利用可能です。

#### Counters

Counterは、型の衝突を起こさないために Riakの新しい `bucket type` 機能を
利用できることを除いて、1.4 の時と同様に振る舞います。
ドキュメントは
[ここ](http://docs.basho.com/riak/2.0.0/dev/using/data-types/#Counters).
にあります。

#### Sets

キーに対して複数の区別できる不透明なバイナリ値の保存ができます。
使用方法と用語に関しては
[ドキュメント](http://docs.basho.com/riak/2.0.0/dev/using/data-types/#Sets)
をご覧ください。


#### Maps

Mapはネストされた再帰的な構造体、もしくは連想配列です。これらを複数のデー
タタイプから構成されるアドホックなデータ構造のコンテナとみなします。
Mapの内部にはSet、Counter、フラグ（booleanの類似物）、レジスタ（Last
Write Winsによりバイナリを保存するもの）や他の Map さえ保存できます。使
用方法と用語に関しては
[ドキュメント](http://docs.basho.com/riak/2.0.0/dev/using/data-types/#Maps)
をご覧ください。

#### API

データ型はこれまでのRiakの操作からのさらなる脱却をもたらします。
それは operation based な API を持っているのです。
これまでは、データ構造を取得し衝突を解消したうえで、その結果を変更し書き戻していましたが、
Riak に対してデータ型へ適用される操作を伝えることになります。

操作の例をいくつか紹介します：

* "counterを10だけ増やす"
* "setへ'joe'を追加する",
* "Mapから 'friends' と呼ばれるSetのフィールドを削除する"
* "Mapの内にある `prepay` フラグを `true` へセットする"


##### Context

データ型を正確に動かすには read によって得た不透明な context を次の場合に
_返さなければなりません_。

* フラグを `false` へ設定した
* Mapからフィールドを削除した
* Setから要素を削除した

この基本ルールは「あなたが見ていないものは削除できない」、そして
context はRiakにあなたが実際に見たものを伝えます。Javaクライアントを除
き、全ての公式Riakクライアントは不透明contextを扱えます。詳細は
[ドキュメント](http://docs.basho.com/riak/2.0.0/dev/using/data-types/#Data-Types-and-Context)
をご覧ください。

Mapに関する既知の不具合が２つあります。 後述する **既知の不具合** に目
を通してください。


### sibling の抑制

以前のバージョンのRiakでは行儀の良いクライアントでさえ、"sibling
explosion"と呼ばれる問題を引き起こすことが普通でした。本質的には、リト
ライや入れ子になった書き込みは、クライアントが書き込む前にsiblingsを解
決したとしても、際限の無いsibling増加をもたらす可能性がありました。
vector clockは各書き込みに付与され、クロックは正確に進みますが、各
siblings値に対する因果関係の情報が失われることにより、この問題が発生し
ていました。同じ書き込みを起源とする値が重複する可能性があったのです。

Riak 2.0ではこの問題に関する Preguiça、Baquero らの
[研究](http://arxiv.org/abs/1011.5808) と
[プロトタイプ](https://github.com/ricardobcl/Dotted-Version-Vectors) の
成果を活かし、これに対処しました。各 sibling の書き込みイベントを表すマー
カー("dot" と呼びます)を付与することにより、siblingsは **実際の並列数
** までしか上昇しません。つまり、オブジェクトの書き込み回数や、マージ回
数、他のクラスタへの複製回数には関係しなくなりました。さらなる情報は
[Dotted Version Vectors](http://docs.basho.com/riak/2.0.0/theory/concepts/dotted-version-vectors/)
のドキュメント参照ください。

### riak_control

* [Ring availabilityページの追加。既存ringページの破棄と問題のあるring状態の表示](https://github.com/basho/riak_control/pull/91)
* [ロードインジケータによるページ遷移の修正](https://github.com/basho/riak_control/pull/159)

### Search 2 (Yokozuna)

Riak Search は、設計から完全に仕切りなおされました。
それは Yokozuna というコードネームで、開発中にメンテナンスされていた個別の
[リリースノート](https://github.com/basho/yokozuna/blob/develop/docs/RELEASE_NOTES.md)
があります。Riak 2.0の新しい検索機能に関連する情報はこちらでご覧ください。
さらに3つの公式ドキュメントがあります。

* [Using Search](http://docs.basho.com/riak/2.0.0/dev/using/search/)
* [Search Details](http://docs.basho.com/riak/2.0.0/dev/advanced/search/)
* [Search Schema](http://docs.basho.com/riak/2.0.0/dev/advanced/search-schema/)

### Strong Consistency

Riak の新しい Strong Consistency 機能はオープンソースとなっており、
Riak Enterprise ではサポートされていません。
公式ドキュメントは次のとおりです。

* [Using Strong Consistency](http://docs.basho.com/riak/2.0.0/dev/advanced/strong-consistency/)
* [Managing Strong Consistency](http://docs.basho.com/riak/2.0.0/ops/advanced/strong-consistency)
* [Strong Consistency](http://docs.basho.com/riak/2.0.0/theory/concepts/strong-consistency/)

さらに深い技術的な情報はインターナルなドキュメントを参照ください。
[ここ](https://github.com/basho/riak_ensemble/blob/wip/riak-2.0-user-docs/riak_consistent_user_docs.md)
と [ここ](https://github.com/basho/riak_ensemble/blob/wip/riak-2.0-user-docs/riak_consistent_user_docs.md).
にあります。

また、[既知の問題](http://docs.basho.com/riak/2.0.0/ops/advanced/strong-consistency/#Known-Issues)
に目を通すことを強く推奨します。

### Security

Riak 2.0 で認証、認可機能を追加されました。これは異なる環境への誤接続防
止（例：開発中のアプリケーションを本番のクラスタに繋いでしまう）や、悪
意のある攻撃への防衛策に役立ちます。無論、依然としてRiakをセキュアでな
いネットワークへ直接公開すべきではありません。

Bashoのドキュメントサイトには
[この新機能の詳細な説明](http://docs.basho.com/riak/2.0.0/ops/running/authz/)
があります。またsecurity機能を有効化する際にいくつかの重要な注意点があ
ります：

* まだ監査機能はサポートされていません。これは今後のロードマップに含ま
  れます。
* ２つの廃止予定の機能はsecurityを有効化すると動作しません。（link
  walking、および旧riak search）
* securityが有効な際に、MapReduceジョブに対して公開されるErlangモジュー
  ルに制限があります。
  その制限は、
  [ここ](http://docs.basho.com/riak/2.0.0/ops/running/authz/#Security-Checklist)
  で説明されています。
* securityを有効化する場合はアプリケーション側の対応が必要です。対応の選択肢は、
  設定変更中のサーバ応答の違いに対し適切に振る舞よう設計するか、
  **もしくは** セキュリティ有効化前にアプリケーションを一旦停止させ、有効化
  後にセキュリティ機能をサポートしたアプリケーションのバージョンをオン
  ラインに戻すことです。

### Packaging / Supported Platforms

2.0 のサポートリストへ多くのプラットフォームが追加されました。

* FreeBSD 10, with new pkgng format
* SUSE SLES 11.2
* Ubuntu 14.04 ('trusty')
* CentOS/RHEL 7

その他、1.4 から既にサポートされていたプラットフォームで更新されたものがあります。

* Fedora packages went from a Fedora 17, to Fedora 19 base
* SmartOS continued to support 1.8 and 13.1 datasets, but dropped 1.6


### Apt/Yum Repositories

我々はaptとyumリポジトリを2.0のユーザーへ提供していますが、パッケージを
提供するサービスへの移行をお願いしています。

**[Packagecloud](https://packagecloud.io/)** はapt/yumリポジトリのホス
ティングに関わる多くのの苦痛を取り払うことはもちろん、ユーザーのみなさ
んに多くの機能を提供する素晴らしいサービスです。みなさんにとって最も重
要な機能はユニバーサルインストーラです。これは対象のOSとバージョンを検
知し、適切なリポジトリとセキュリティキーの自動インストールを提供します。

2.0 パッケージは Packagecloud にホストされる一方で、いまのところ 1.4 パッ
ケージは[apt|yum].basho.comに残ります。我々は追加機能によるメリットが、
みなさんのツール内のURL更新に伴うあらゆる苦痛を補うことを願っています。
このような変更をして申し訳ありませんが、これは今後への良き投資であると
考えています。

## Client libraries

ほぼすべての
[Basho サポートのクライアントライブラリ](http://docs.basho.com/riak/latest/dev/using/libraries/)
は、2.0 用に更新されています。

* [Java](https://github.com/basho/riak-java-client)
* [Ruby](https://github.com/basho/riak-ruby-client)
* [Python](https://github.com/basho/riak-python-client)
* [Erlang](https://github.com/basho/riak-erlang-client)

PHP ライブラリはまだ更新が終わっていません。予定は決まっていません。

### Bitcask

* 今回データ走査に使うイテレータの複数稼働が可能になりました。以前、
  Bitcaskのデータ走査イテレータは一つだけで、これはAAEやfullsyncの処理
  をブロックする可能性がありました。また本リリースでは、インメモリのキー
  ディレクトリは複数のスナップショットが共存し、１つのエントリに複数の
  値を持つように修正されました。これはイテレータが頻繁に使われる際に、
  メモリを以前より消費することを意味します。
* リスタート後に削除済みの値が復活する問題を修正。新しいtombstoneフォー
  マットと削除アルゴリズムに対応するため、ヒントファイルとデータファイ
  ルのフォーマットが変更されました。マージアルゴリズムの削除マークが付
  与されたファイルは、今回setuidビットの代わりにexecutionビットがセット
  されます。フォーマット変更により、ダウングレードする場合は旧バージョ
  ンのRiakは新フォーマットのファイル読み込みに失敗するため、ヒントファ
  イルを削除する必要があります。Riakは新フォーマットファイルを再生成す
  るため、全てのBitcaskファイルの段階的なマージを実施します。このマージ
  はmerge window設定に従い、ノードが高負荷にならないように分割して実行
  されます。マージは完全にスキップもしくは調整する設定項目がいくつか用
  意されています。Bitcaskはマージしてもしなくても正常に動作します。以前
  の旧フォーマットファイルの領域解放よりも、Bitcaskが多くの時間を必要と
  するようになるため、できるだけ早くディスクスペースを解放することがこ
  の設定の狙いです。
* 起動中のマージに関する問題を修正。マージは `riak_kv` サービスの起動ま
  で延期されます。

### HTTP API

歴史的に、Basho のライブラリは Riak アクセスのための HTTP とプロトコルバッファ
をどちらもサポートしてきました。
最近まで、HTTP はすべての Riak の機能をサポートしている強みがありました。

今ではプロトコルバッファが機能的に同等となり、またプロトコルバッファが
一般にはより速いことから、Basho は **クライアントライブラリだけからは**
HTTP サポートを取り除きつつあります。
データベースとしての HTTP API を削除する予定はありません。

Python クライアントは HTTP サポートを残していますが、Java、Ruby、
Erlang は HTTP をサポートしません。

### 廃止予定の機能

Riak 2.0 はいくつかの機能を廃止予定とします。
後述する **廃止される機能** もご覧ください。

* [Link Walking](http://docs.basho.com/riak/latest/dev/using/link-walking/)
  は廃止予定となり、security が有効化されると動作しません。
* [Key Filters](http://docs.basho.com/riak/latest/dev/using/keyfilters/)
  は廃止予定です。key listing は、その負荷のため、プロダクション環境で
  使うべきではありません。たとえば、キーインデックスを Riak のバリューとして
  保存するほうが良い選択肢です。
  (このようなインデックスには、新しい
  [set data type](http://docs.basho.com/riak/2.0.0/dev/using/data-types/#Sets)
  が有用です)
* JavaScript MapReduce は廃止予定です。移行を手助けするため、
  [Erlang MapReduce](http://docs.basho.com/riak/2.0.0/dev/advanced/mapreduce/)
  ドキュメントはすでに拡充済みです。
* Riak Search 1.0 は新しい Solr ベースの
  [Riak Search 2.0](http://docs.basho.com/riak/2.0.0/dev/advanced/search/)
  を選択したため、段階的に廃止されます。
  バージョン 1.0 は security が有効化されると動作しません。
* v2 レプリケーション (Riak Enterprise の機能です)は v3 に置き換えられ、
  将来削除予定です。
* レガシーゴシップ(Riak の最初のゴシップ機構でしたが、1.0 で置き換えられました)
  は将来削除予定です。
  その時点で 1.0 以前の Riak ノードはクラスタに join 出来なくなります。
* レガシー vnode ルーティング (サーバ間のリクエスト管理のための初期の仕
  組み)は廃止予定です。もし `vnode_routing` が Riak capability システム
  により `legacy` に設定されている場合、将来のアップグレードでの問題を
  防ぐため、それを取り除くべきです。
* 過去に Riak の内部 API (例 `riak:local_client/1`) を使っているユーザ
  がいましたが、この API はいつでも変更される可能性があります。代わりに
  [Erlang client library](http://github.com/basho/riak-erlang-client/)
  (もしくは
  [いずれかのクライアントライブラリ](http://docs.basho.com/riak/latest/dev/using/libraries/))
  を利用することを強く推奨します。

## 廃止される機能

* `riak-admin backup` は廃止されました。バックアップとリストアについて
  の詳細な手順は
  [ドキュメント](http://docs.basho.com/riak/2.0.0/ops/running/backups/)
  をご覧ください。
* [Client ID-based vector clocks](http://docs.basho.com/riak/1.4.10/ops/advanced/configs/configuration-files/#-code-riak_kv-code-Settings)
  は削除されました。
  ノードベースの vector clock がより好ましいことから、`vnode_vclocks` 設定により
  この機能は以前からデフォルトで停止されていました。
* LevelDB の設定から `cache_size` と `max_open_files` が廃止されました。
  代わりに `leveldb.maximum_memory.percent` を使ってください。詳細は
  [Configuring eLevelDB](http://docs.basho.com/riak/2.0.0/ops/advanced/backends/leveldb/#Configuring-eLevelDB)
  をご覧ください。

## 既知の問題

バージョン 2.0 での既知の問題は
[この Riak wiki ページ](https://github.com/basho/riak/wiki/2.0-known-issues). で
完全なリストを見ることが出来ます。

## アップグレードノート

2.0 へアップグレードする完全なドキュメントは
[公式のページ](http://docs.basho.com/riak/2.0.0/upgrade-v20/) にあります。
以下の情報はそれを補完するものです。

### インストール後のダウングレード

**重要**: 2.0 は主要な新機能を導入します。そのうちのいくつかは
Riak 1.x と互換ではありません。
そのような機能は
[bucket types](http://docs.basho.com/riak/2.0.0/dev/advanced/bucket-types/)
に依存しています。
バケットタイプが *ひとつでも* 作成され有効化された後には、ダウングレードは
できません。

Riak 1.x へダウングレードする前には、必要な手順についての情報のため
[2.0 downgrade notes](https://github.com/basho/riak/wiki/2.0-downgrade-notes)
を参照してください。

#### 設定ファイル

1.4 および以前の設定ファイル (`app.config` and `vm.args`) から2.0の
新しい設定ファイルへの変換は自動でできません。以前
の設定ファイル `app.config` と `vm.args` は、設定ファイルのディレクトリ
内にあれば動作しますが、設定のカスタマイズ内容を `riak.conf` と
`advanced.config` へ反映させ、今後の設定を簡単にすることをお勧めします。
追加の情報は
[configuration files documentation](http://docs.basho.com/riak/2.0.0/ops/advanced/configs/configuration-files/)
にあります。

## バグフィックス / 1.4.x からの変更

下記のリストは1.4.x 2.0間でマージされた全てのPRです。2.0 で追加された次
のリポジトリはこれに含みません。リストに加えてこれらのリポジトリからの
全てのPRにも注意して下さい。

#### 2.0 で追加されたリポジトリ

* [**Canola** -  PAM driver for Erlang](https://github.com/basho/canola)
* [**Cuttlefish** -  Riak's new configuration tool](https://github.com/basho/cuttlefish)
* [**pbkdf2** -  PBKDF2 implementation for Erlang](https://github.com/basho/erlang-pbkdf2)
* [**riak_auth_mods** -  Security authentication modules for Riak](https://github.com/basho/riak_auth_mods)
* [**riak_dt** -  Convergent replicated datatypes (CRDTs) in Erlang](https://github.com/basho/riak_dt)
* [**riak_ensemble** -  Multi-Paxos framework in Erlang](https://github.com/basho/riak_ensemble)
* [**Yokozuna** -  Riak Search 2, Riak + Solr](https://github.com/basho/yokozuna)

#### マージ済PR

* bitcask/103: [add optional key transformer to support new key formats](https://github.com/basho/bitcask/pull/103)
* bitcask/104: [Improve bitcask iteration concurrency.](https://github.com/basho/bitcask/pull/104)
* bitcask/106: [Refactor of Evan's multifold](https://github.com/basho/bitcask/pull/106)
* bitcask/110: [remove unused header](https://github.com/basho/bitcask/pull/110)
* bitcask/112: [moved in bitcask schema bits from riak.schema](https://github.com/basho/bitcask/pull/112)
* bitcask/115: [lazily create merge files to avoid creation of empty files](https://github.com/basho/bitcask/pull/115)
* bitcask/116: [Fix fstat struct leak.](https://github.com/basho/bitcask/pull/116)
* bitcask/118: [avoid file server](https://github.com/basho/bitcask/pull/118)
* bitcask/119: [added erlang file header.](https://github.com/basho/bitcask/pull/119)
* bitcask/123: [Refactor bitcask_fileops:fold_keys function](https://github.com/basho/bitcask/pull/123)
* bitcask/124: [Add a Makefile target for pulse tests](https://github.com/basho/bitcask/pull/124)
* bitcask/125: [Updated schema for new cuttlefish api](https://github.com/basho/bitcask/pull/125)
* bitcask/127: [Rename/refactor a bunch of cuttlefish settings.](https://github.com/basho/bitcask/pull/127)
* bitcask/130: [fix bitcask.schema for multi_backend](https://github.com/basho/bitcask/pull/130)
* bitcask/132: [Use tools.mk for dialyzer support](https://github.com/basho/bitcask/pull/132)
* bitcask/133: [Add xref target](https://github.com/basho/bitcask/pull/133)
* bitcask/135: [Changed bitcask.data_root to directory datatype](https://github.com/basho/bitcask/pull/135)
* bitcask/138: [Cuttlefish schema RHS sub for platform_bin_dir](https://github.com/basho/bitcask/pull/138)
* bitcask/139: [Dialyzer fixes and turn on warn_untyped_record & warnings_as_errors](https://github.com/basho/bitcask/pull/139)
* bitcask/140: [move from timestamps to epochs for folding and siblings](https://github.com/basho/bitcask/pull/140)
* bitcask/141: [Changed cuttlefish rhs subs to use $ syntax](https://github.com/basho/bitcask/pull/141)
* bitcask/143: [Simplify find entry snapshot use](https://github.com/basho/bitcask/pull/143)
* bitcask/144: [Add sibling->regular entry conversion sweeper](https://github.com/basho/bitcask/pull/144)
* bitcask/145: [Ensure licensure for long pulse tests.](https://github.com/basho/bitcask/pull/145)
* bitcask/147: [Fix potential keyfolders count leak](https://github.com/basho/bitcask/pull/147)
* bitcask/148: [Fix race with concurrent merges and deletes](https://github.com/basho/bitcask/pull/148)
* bitcask/150: [Remove the possibility for merge and open to race.](https://github.com/basho/bitcask/pull/150)
* bitcask/151: [multifold test stabilization](https://github.com/basho/bitcask/pull/151)
* bitcask/155: [Set the hash symbol to dollars](https://github.com/basho/bitcask/pull/155)
* bitcask/157: [Fixed an issue with some multi_backend versions not matching regular versions](https://github.com/basho/bitcask/pull/157)
* bitcask/158: [Restore fix to timestamp test](https://github.com/basho/bitcask/pull/158)
* bitcask/160: [add warnings as errors to the nif build flags](https://github.com/basho/bitcask/pull/160)
* bitcask/161: [Fixes for 'faulterl'-style fault injection](https://github.com/basho/bitcask/pull/161)
* bitcask/162: [Pevm pulse tweaks multifold2](https://github.com/basho/bitcask/pull/162)
* bitcask/164: [Invalid hintfile error message is too severe](https://github.com/basho/bitcask/pull/164)
* bitcask/170: [Deferred delete bug (aka Cd8)](https://github.com/basho/bitcask/pull/170)
* bitcask/173: [forward port of fix for fold_file_loop](https://github.com/basho/bitcask/pull/173)
* bitcask/175: [Bugfix/fold open delete race](https://github.com/basho/bitcask/pull/175)
* bitcask/177: [Fix epoch comparison by find_keydir_entry() when keydir->pending != NULL](https://github.com/basho/bitcask/pull/177)
* bitcask/179: [Update tools.mk to v0.5.5](https://github.com/basho/bitcask/pull/179)
* cluster_info/13: [use lager_format:format/4 if available](https://github.com/basho/cluster_info/pull/13)
* cluster_info/14: [Use tools.mk Makefile](https://github.com/basho/cluster_info/pull/14)
* ebloom/10: [Use tools.mk Makefile](https://github.com/basho/ebloom/pull/10)
* eleveldb/103: [Change cuttlefish RHS sub to $ syntax](https://github.com/basho/eleveldb/pull/103)
* eleveldb/104: [correct prefetch race condition.](https://github.com/basho/eleveldb/pull/104)
* eleveldb/105: [Fix dialyzer and xref errors](https://github.com/basho/eleveldb/pull/105)
* eleveldb/109: [Mv compress option](https://github.com/basho/eleveldb/pull/109)
* eleveldb/111: [Mv tiered options](https://github.com/basho/eleveldb/pull/111)
* eleveldb/113: [avoid badarg exception when closing db_refs being closed](https://github.com/basho/eleveldb/pull/113)
* eleveldb/117: [Mv iter close fix](https://github.com/basho/eleveldb/pull/117)
* eleveldb/119: [Must wait until complete close of iterator finishes. ](https://github.com/basho/eleveldb/pull/119)
* eleveldb/69: [mv-iterator-prev branch](https://github.com/basho/eleveldb/pull/69)
* eleveldb/70: [Specify the Snappy libdir install location](https://github.com/basho/eleveldb/pull/70)
* eleveldb/73: [Mv flexcache](https://github.com/basho/eleveldb/pull/73)
* eleveldb/74: [add limited_developer_mem option flag (support)](https://github.com/basho/eleveldb/pull/74)
* eleveldb/75: [Mv flexcache4](https://github.com/basho/eleveldb/pull/75)
* eleveldb/77: [fixed dialyzer errors in async_iterator_move](https://github.com/basho/eleveldb/pull/77)
* eleveldb/80: [multibackend part of .schema belongs into riak_kv](https://github.com/basho/eleveldb/pull/80)
* eleveldb/87: [Schema updates for new cuttlefish API. unit test.](https://github.com/basho/eleveldb/pull/87)
* eleveldb/88: [Mv iterator refresh](https://github.com/basho/eleveldb/pull/88)
* eleveldb/89: [Schema Changes](https://github.com/basho/eleveldb/pull/89)
* eleveldb/92: [Use tools.mk Makefile](https://github.com/basho/eleveldb/pull/92)
* eleveldb/93: [Mv tuning4](https://github.com/basho/eleveldb/pull/93)
* eleveldb/95: [Cuttlefish rhs subs for platform_bin_dir](https://github.com/basho/eleveldb/pull/95)
* eleveldb/96: [Mv tuning6](https://github.com/basho/eleveldb/pull/96)
* eper/10: [Merge pull request #1 from basho/master](https://github.com/basho/eper/pull/10)
* eper/5: [Update Makefile so that it automatically pulls the new deps.](https://github.com/basho/eper/pull/5)
* eper/6: [R16B01 compatibility changes.](https://github.com/basho/eper/pull/6)
* eper/6: [R16B01 compatibility changes.](https://github.com/basho/eper/pull/6)
* eper/7: [Additional R16B01 compatibility changes.](https://github.com/basho/eper/pull/7)
* eper/8: [Add Dialyzer support via tools.mk Makefile](https://github.com/basho/eper/pull/8)
* eper/9: [Merge upstream 0.78](https://github.com/basho/eper/pull/9)
* erlang_js/34: [Add port_spec due to rebar not adding a default port_spec any more](https://github.com/basho/erlang_js/pull/34)
* erlang_js/35: [Support non-binary error-reasons in define_js/4](https://github.com/basho/erlang_js/pull/35)
* erlang_js/37: [Add patch to js-1.8.0 which fixes inline compilation error with gcc 4.7+](https://github.com/basho/erlang_js/pull/37)
* erlang_js/38: [Dialyzer](https://github.com/basho/erlang_js/pull/38)
* erlang_js/39: [Resolve build problems on Mountain Lion.](https://github.com/basho/erlang_js/pull/39)
* erlang_js/40: [Regardless of OTP rel, build Mountain Lion 64-bit.](https://github.com/basho/erlang_js/pull/40)
* erlang_js/43: [Permit quotes in anonymous functions](https://github.com/basho/erlang_js/pull/43)
* lager/170: [pretty printing of nested records](https://github.com/basho/lager/pull/170)
* lager/179: [Fix lager eunit initialization](https://github.com/basho/lager/pull/179)
* lager/185: [Fixed formatting.](https://github.com/basho/lager/pull/185)
* lager/186: [Allow PLT destination to be specified](https://github.com/basho/lager/pull/186)
* lager/187: [Rework how dialyzer PLTs are built and used](https://github.com/basho/lager/pull/187)
* lager/188: [Fix dialyzer warnings and make all the records typed](https://github.com/basho/lager/pull/188)
* lager/192: [Remove the workaround for the bug when printing empty binaries in W mode](https://github.com/basho/lager/pull/192)
* lager/193: [Fix lager_console_backend:is_new_style_console_available() function](https://github.com/basho/lager/pull/193)
* lager/194: [Add xref target, with exclusions](https://github.com/basho/lager/pull/194)
* lager/196: [Don't use the proplists module when decoding error_logger messages](https://github.com/basho/lager/pull/196)
* lager/197: [Add newline to error_msg:error_report lines in crash.log, see #164](https://github.com/basho/lager/pull/197)
* lager/199: [Fixed empty tuple bug in lager:pr/2](https://github.com/basho/lager/pull/199)
* lager/200: [Fix: correct lager startup in crash_log test](https://github.com/basho/lager/pull/200)
* lager/201: [Only discard gen_event notifications on high watermark](https://github.com/basho/lager/pull/201)
* lager/202: [Make tests pass on buildbot more of the time](https://github.com/basho/lager/pull/202)
* lager/204: [support disable pretty printing records encountered at compile time](https://github.com/basho/lager/pull/204)
* lager_syslog/10: [updated readme for clarity](https://github.com/basho/lager_syslog/pull/10)
* lager_syslog/9: [Improve lager_syslog_backend to support non-atom output log levels.](https://github.com/basho/lager_syslog/pull/9)
* leveldb/90: [leveldb's mv-clean-overlaps](https://github.com/basho/leveldb/pull/90)
* leveldb/92: [Mv spin locks](https://github.com/basho/leveldb/pull/92)
* leveldb/93: [Mv throttle 4](https://github.com/basho/leveldb/pull/93)
* leveldb/95: [Mv flexcache](https://github.com/basho/leveldb/pull/95)
* leveldb/96: [Fix version_set compilation bug](https://github.com/basho/leveldb/pull/96)
* leveldb/98: [Mv counters update](https://github.com/basho/leveldb/pull/98)
* leveldb/99: [Mv async close](https://github.com/basho/leveldb/pull/99)
* leveldb/100: [Mv flexcache2](https://github.com/basho/leveldb/pull/100)
* leveldb/101: [Mv hot threads1](https://github.com/basho/leveldb/pull/101)
* leveldb/102: [revert to original, fixed 20 percent for internal databasses](https://github.com/basho/leveldb/pull/102)
* leveldb/103: [install additional performance counters](https://github.com/basho/leveldb/pull/103)
* leveldb/104: [change build_detect_platform to check for OS X Mavericks](https://github.com/basho/leveldb/pull/104)
* leveldb/105: [Mv flexcache5](https://github.com/basho/leveldb/pull/105)
* leveldb/106: [Mv hot threads2](https://github.com/basho/leveldb/pull/106)
* leveldb/108: [Mv aggressive delete](https://github.com/basho/leveldb/pull/108)
* leveldb/111: [Mv dynamic block size](https://github.com/basho/leveldb/pull/111)
* leveldb/113: [Mv fadvise control 2.0](https://github.com/basho/leveldb/pull/113)
* leveldb/114: [mv-iterator-refresh (part 2)](https://github.com/basho/leveldb/pull/114)
* leveldb/117: [mv-tuning4](https://github.com/basho/leveldb/pull/117)
* leveldb/119: [mv tuning5](https://github.com/basho/leveldb/pull/119)
* leveldb/120: [Mv tuning6](https://github.com/basho/leveldb/pull/120)
* leveldb/125: [activate the AssertHeld() logic of Mutex and Spin classes. And address issue #100](https://github.com/basho/leveldb/pull/125)
* leveldb/129: [Two fail case fixes from failure injection tests](https://github.com/basho/leveldb/pull/129)
* leveldb/130: [Mv tiered options](https://github.com/basho/leveldb/pull/130)
* leveldb/131: [code to isolate Log() file flushes from happening within mutex...](https://github.com/basho/leveldb/pull/131)
* leveldb/132: [Mv write sizing](https://github.com/basho/leveldb/pull/132)
* leveldb/137: [Mv delete mutex fix](https://github.com/basho/leveldb/pull/137)
* leveldb/139: [Mv tuning9](https://github.com/basho/leveldb/pull/139)
* leveldb/140: [Give Read and Iterator calls more consistent disk access on moderately loaded systems.](https://github.com/basho/leveldb/pull/140)
* merge_index/27: [Cv 3717 patch](https://github.com/basho/merge_index/pull/27)
* node_package/100: [Explicitly set a destination dir for generated files by cuttlefish](https://github.com/basho/node_package/pull/100)
* node_package/102: [Add optional support for specifying a NUMA policy](https://github.com/basho/node_package/pull/102)
* node_package/103: [added extra -vm_args to CONFIG_ARGS for easy access by erlang vm](https://github.com/basho/node_package/pull/103)
* node_package/106: [added support for extra cuttlefish commands](https://github.com/basho/node_package/pull/106)
* node_package/108: [Ensure word-splitting does not happen when re-running as other user.](https://github.com/basho/node_package/pull/108)
* node_package/109: [Fix patch for smartos to add quotes](https://github.com/basho/node_package/pull/109)
* node_package/112: [Add chkconfig to install and uninstall scripts for RPMs](https://github.com/basho/node_package/pull/112)
* node_package/113: [Add support for -kernel net_ticktime](https://github.com/basho/node_package/pull/113)
* node_package/116: [Read /etc/sysconfig/<service> file in RHEL/Fedora init script](https://github.com/basho/node_package/pull/116)
* node_package/119: [force nodetool's encoding to be unicode](https://github.com/basho/node_package/pull/119)
* node_package/121: [Change prctl calls from `-t basic` to `-t system` for SmartOS](https://github.com/basho/node_package/pull/121)
* node_package/123: [Further support for cuttlefish configuration files in SmartOS](https://github.com/basho/node_package/pull/123)
* node_package/126: [Replace sudo in runner scripts with su for greater compatibility](https://github.com/basho/node_package/pull/126)
* node_package/128: [Add packaging for FreeBSD pkg-ng](https://github.com/basho/node_package/pull/128)
* node_package/129: [Escape quotes passed on runner command line](https://github.com/basho/node_package/pull/129)
* node_package/132: [Remove reboot from runner script](https://github.com/basho/node_package/pull/132)
* node_package/134: [Escape '{' and '}' in env.sh before calling su](https://github.com/basho/node_package/pull/134)
* node_package/137: [Set HOME env var and proper perms on nodetool in Ubuntu](https://github.com/basho/node_package/pull/137)
* node_package/140: [Add basic support for SuSE Linux](https://github.com/basho/node_package/pull/140)
* node_package/143: [Restart the old-fashioned way](https://github.com/basho/node_package/pull/143)
* node_package/146: [Require root or runner privs to run `ping` command](https://github.com/basho/node_package/pull/146)
* node_package/148: [Cleanup usage documentation for chkconfig](https://github.com/basho/node_package/pull/148)
* node_package/149: [Source default config files in env.sh](https://github.com/basho/node_package/pull/149)
* node_package/78: [node_package cuttlefish integration](https://github.com/basho/node_package/pull/78)
* node_package/80: [Added 'help' section](https://github.com/basho/node_package/pull/80)
* node_package/81: [Fixed issues with whitespace for non-cuttlefish configs](https://github.com/basho/node_package/pull/81)
* node_package/85: [make chkconfig output path to config file](https://github.com/basho/node_package/pull/85)
* node_package/86: [vm.args support for node_package](https://github.com/basho/node_package/pull/86)
* node_package/87: [Fixes 'attach' in runner script](https://github.com/basho/node_package/pull/87)
* node_package/88: [Remove the output of ping from the console command](https://github.com/basho/node_package/pull/88)
* node_package/89: [Fix return code of service stop in the RPM init script](https://github.com/basho/node_package/pull/89)
* node_package/92: [fixed node and cookie regex to be more inclusive, more whitespace](https://github.com/basho/node_package/pull/92)
* node_package/96: [Use shell globing instead of unnecessary and dangerous ls construct.](https://github.com/basho/node_package/pull/96)
* riak_api/31: [Add swap/3 API](https://github.com/basho/riak_api/pull/31)
* riak_api/33: [Fix case where the registrar is not the table owner but a swap message is sent.](https://github.com/basho/riak_api/pull/33)
* riak_api/34: [Move webmachine from riak_core.](https://github.com/basho/riak_api/pull/34)
* riak_api/35: [Add security to Riak](https://github.com/basho/riak_api/pull/35)
* riak_api/36: [pb_service_test no longer runs](https://github.com/basho/riak_api/pull/36)
* riak_api/38: [Moved riak_api bits of riak.schema, and added unit tests](https://github.com/basho/riak_api/pull/38)
* riak_api/39: [Add CRL checking for client certificate](https://github.com/basho/riak_api/pull/39)
* riak_api/40: [Reformatting.](https://github.com/basho/riak_api/pull/40)
* riak_api/43: [Added protobuf.nagle to the riak_api.schema](https://github.com/basho/riak_api/pull/43)
* riak_api/44: [Fix late registrations: riak_api_pb_server is no longer a gen_server](https://github.com/basho/riak_api/pull/44)
* riak_api/46: [Honor configured cipher suites and add an option to honor the order](https://github.com/basho/riak_api/pull/46)
* riak_api/47: [Changes for new cuttlefish API](https://github.com/basho/riak_api/pull/47)
* riak_api/48: [Remove references to ranch_tcp and ranch_ssl.](https://github.com/basho/riak_api/pull/48)
* riak_api/49: [Remove reset functionality from bucket-type service.](https://github.com/basho/riak_api/pull/49)
* riak_api/51: [Confbal/schema review and tests](https://github.com/basho/riak_api/pull/51)
* riak_api/52: [Add tools.mk](https://github.com/basho/riak_api/pull/52)
* riak_api/53: [Refactor riak_api_pb_service to use callback mod attributes](https://github.com/basho/riak_api/pull/53)
* riak_api/54: [HAproxy health-check causes node shutdown](https://github.com/basho/riak_api/pull/54)
* riak_api/55: [Add xref target and fix uncovered bug](https://github.com/basho/riak_api/pull/55)
* riak_api/56: [{level, advanced} -> hidden](https://github.com/basho/riak_api/pull/56)
* riak_api/57: [No longer call deprecated function](https://github.com/basho/riak_api/pull/57)
* riak_api/58: [Change error message returned when no module reg.](https://github.com/basho/riak_api/pull/58)
* riak_api/59: [Fix dialyzer warnings](https://github.com/basho/riak_api/pull/59)
* riak_api/60: [Make HTTPS consistent with PB](https://github.com/basho/riak_api/pull/60)
* riak_api/62: [Handle the {error, no_type} return from riak_core_bucket.](https://github.com/basho/riak_api/pull/62)
* riak_control/112: [Fix redirect logic.](https://github.com/basho/riak_control/pull/112)
* riak_control/114: [Add the ability to select all available n_vals in cluster.](https://github.com/basho/riak_control/pull/114)
* riak_control/126: [Handle incompatible record in Riak 1.4.0.](https://github.com/basho/riak_control/pull/126)
* riak_control/136: [Incompatible is less serious (master)](https://github.com/basho/riak_control/pull/136)
* riak_control/138: [Merge 1.4 into master and reconcile changes.](https://github.com/basho/riak_control/pull/138)
* riak_control/142: [Fix cherry-pick conflict in templates.js](https://github.com/basho/riak_control/pull/142)
* riak_control/143: [Upgrade to ember-1.0.0 and the latest ember-data beta](https://github.com/basho/riak_control/pull/143)
* riak_control/146: [Fix bad function call.](https://github.com/basho/riak_control/pull/146)
* riak_control/147: [Bring templates up-to-date.](https://github.com/basho/riak_control/pull/147)
* riak_control/150: [Remove generated files.  ](https://github.com/basho/riak_control/pull/150)
* riak_control/154: [Fix bad paths and ignore generated files.](https://github.com/basho/riak_control/pull/154)
* riak_control/155: [Use webmachine develop branch.](https://github.com/basho/riak_control/pull/155)
* riak_control/159: [Use promises to engage loadingRoute on all application states.](https://github.com/basho/riak_control/pull/159)
* riak_control/160: [Fix rebar.config to use {branch, "name"}](https://github.com/basho/riak_control/pull/160)
* riak_control/161: [Add testing harness.](https://github.com/basho/riak_control/pull/161)
* riak_control/164: [Add force_ssl flag to Riak Control.](https://github.com/basho/riak_control/pull/164)
* riak_control/165: [riak_control cuttlefish schema and tests](https://github.com/basho/riak_control/pull/165)
* riak_control/166: [No longer build for R14.](https://github.com/basho/riak_control/pull/166)
* riak_control/167: [Address regression introduced by basho/riak#403.](https://github.com/basho/riak_control/pull/167)
* riak_control/168: [Deprecate unused, failing, tests.](https://github.com/basho/riak_control/pull/168)
* riak_control/169: [Replace README.](https://github.com/basho/riak_control/pull/169)
* riak_control/171: [Fix dropdown for replace not not populating correctly](https://github.com/basho/riak_control/pull/171)
* riak_control/174: [Provide some basic tests around the RiakControl Main Pages](https://github.com/basho/riak_control/pull/174)
* riak_control/176: [Changes for new cuttlefish api](https://github.com/basho/riak_control/pull/176)
* riak_control/177: [Simplified riak_control's cuttlefish schema. CONFBAL STYLE!](https://github.com/basho/riak_control/pull/177)
* riak_control/178: [Sensible defaults for riak_control](https://github.com/basho/riak_control/pull/178)
* riak_control/180: [Fix for Issue 179](https://github.com/basho/riak_control/pull/180)
* riak_control/181: [try/catch for rex error](https://github.com/basho/riak_control/pull/181)
* riak_control/91: [Add new ring availability page.](https://github.com/basho/riak_control/pull/91)
* riak_control/93: [Add doc; general cleanup.](https://github.com/basho/riak_control/pull/93)
* riak_control/94: [Add additional specs.](https://github.com/basho/riak_control/pull/94)
* riak_core/285: [Remove unnecessary function call.](https://github.com/basho/riak_core/pull/285)
* riak_core/348: [R16B01 compatibility changes.](https://github.com/basho/riak_core/pull/348)
* riak_core/363: [Cluster Metadata (Part 1/2)](https://github.com/basho/riak_core/pull/363)
* riak_core/367: [Fix meck related unit test problems](https://github.com/basho/riak_core/pull/367)
* riak_core/368: [Merge 1.4.2 to develop](https://github.com/basho/riak_core/pull/368)
* riak_core/369: [Use raw ring when setting bucket props (develop branch)](https://github.com/basho/riak_core/pull/369)
* riak_core/381: [Remove webmachine dependency](https://github.com/basho/riak_core/pull/381)
* riak_core/382: [Remove protobuffs dependency from riak_core.](https://github.com/basho/riak_core/pull/382)
* riak_core/386: [Add security to Riak](https://github.com/basho/riak_core/pull/386)
* riak_core/387: [Expose the resolution of capabilities for general use in protocol negotiations](https://github.com/basho/riak_core/pull/387)
* riak_core/393: [update hashtree_eqc. get it passing on eqc 1.29.1](https://github.com/basho/riak_core/pull/393)
* riak_core/394: [Fix rebar.config to use {branch, "name"}](https://github.com/basho/riak_core/pull/394)
* riak_core/396: [address some metadata hashtree issues](https://github.com/basho/riak_core/pull/396)
* riak_core/397: [dialyzer fixes for cluster metadata](https://github.com/basho/riak_core/pull/397)
* riak_core/398: [Remove merkle](https://github.com/basho/riak_core/pull/398)
* riak_core/400: [Fixes #189 -- include socket peer info in lager messages](https://github.com/basho/riak_core/pull/400)
* riak_core/402: [Proper Bucket Type Creation](https://github.com/basho/riak_core/pull/402)
* riak_core/408: [mark AAE vnodes as internal databases for leveldb flexcache accounting](https://github.com/basho/riak_core/pull/408)
* riak_core/410: [export vsn of riak_core_ring:future_index that doesn't take ring](https://github.com/basho/riak_core/pull/410)
* riak_core/411: [Sort AAE differences before acting upon them via read-repair](https://github.com/basho/riak_core/pull/411)
* riak_core/413: [Fix eunit test failures](https://github.com/basho/riak_core/pull/413)
* riak_core/414: [Increase disterl buffer sizes.](https://github.com/basho/riak_core/pull/414)
* riak_core/415: [Add infrastructure to change the OTP net_kernel's net_ticktime](https://github.com/basho/riak_core/pull/415)
* riak_core/416: [move ebin/riak_core.app to src/riak_core.app.src](https://github.com/basho/riak_core/pull/416)
* riak_core/417: [refactored riak_core bits of the schema in here, with tests](https://github.com/basho/riak_core/pull/417)
* riak_core/421: [Update otp_release list in .travis.yml](https://github.com/basho/riak_core/pull/421)
* riak_core/423: [fill in missing props in riak_core_bucket_type:defaults/0](https://github.com/basho/riak_core/pull/423)
* riak_core/424: [moved default bucket props from riak_kv](https://github.com/basho/riak_core/pull/424)
* riak_core/425: [GET ON THIS LEVEL](https://github.com/basho/riak_core/pull/425)
* riak_core/430: [Fix bad type specifications.](https://github.com/basho/riak_core/pull/430)
* riak_core/431: [do not serialize through gen_server on metadata get](https://github.com/basho/riak_core/pull/431)
* riak_core/432: [Make load_certs function exported and more useful (works on a single file, too)](https://github.com/basho/riak_core/pull/432)
* riak_core/433: [share segment store accross all nodes in hashtree_tree](https://github.com/basho/riak_core/pull/433)
* riak_core/435: [Rework table formatting to be better and output better info](https://github.com/basho/riak_core/pull/435)
* riak_core/436: [2.0 version of handoff backwards compatability](https://github.com/basho/riak_core/pull/436)
* riak_core/437: [Implement riak security enable/disable/status](https://github.com/basho/riak_core/pull/437)
* riak_core/441: [Support for strongly consistent Riak](https://github.com/basho/riak_core/pull/441)
* riak_core/445: [initial add of riak_core_bucket_types.hrl](https://github.com/basho/riak_core/pull/445)
* riak_core/446: [cleanup conn_mgr/service_mgr eunit output](https://github.com/basho/riak_core/pull/446)
* riak_core/447: [Removed webmachine from applications that should be started.](https://github.com/basho/riak_core/pull/447)
* riak_core/451: [Implement alter-user, del-user and del-source](https://github.com/basho/riak_core/pull/451)
* riak_core/453: [add missing apps to src/riak_kv.app.src](https://github.com/basho/riak_core/pull/453)
* riak_core/454: [Added handoff concurrency setting to schema.](https://github.com/basho/riak_core/pull/454)
* riak_core/457: [Yet Another Round of Cluster Metadata Improvements (YAROCMI #1)](https://github.com/basho/riak_core/pull/457)
* riak_core/460: [Validate Bucket Type Properties Required by Core](https://github.com/basho/riak_core/pull/460)
* riak_core/462: [Add EQC statem property for vclock. [rebased]](https://github.com/basho/riak_core/pull/462)
* riak_core/463: [Add get dot and dot type for dvv style causality](https://github.com/basho/riak_core/pull/463)
* riak_core/467: [Bound the time that stats calculation can take](https://github.com/basho/riak_core/pull/467)
* riak_core/468: [Changing how pathing is determined:](https://github.com/basho/riak_core/pull/468)
* riak_core/469: [Make cipher suites configurable via the command line](https://github.com/basho/riak_core/pull/469)
* riak_core/471: [Provide a synchronous registration and unregistration of services.](https://github.com/basho/riak_core/pull/471)
* riak_core/478: [New cuttlefish api functions](https://github.com/basho/riak_core/pull/478)
* riak_core/480: [Port 1.4 changes to the 2.0 branch.](https://github.com/basho/riak_core/pull/480)
* riak_core/482: [Ensure stats progress and tag individual stale stats](https://github.com/basho/riak_core/pull/482)
* riak_core/483: [Numerous AAE improvements](https://github.com/basho/riak_core/pull/483)
* riak_core/484: [Background Manager Integration with Handoff](https://github.com/basho/riak_core/pull/484)
* riak_core/486: [riak_core support for kv#734](https://github.com/basho/riak_core/pull/486)
* riak_core/488: [Wildcard security sources overlapping with regular user sources causes issues](https://github.com/basho/riak_core/pull/488)
* riak_core/490: [Fix mailing list link in README](https://github.com/basho/riak_core/pull/490)
* riak_core/491: [remove legacy forwarding code that snuck in as part of 632af2b3ce9](https://github.com/basho/riak_core/pull/491)
* riak_core/495: [Filter Typed Bucket Tombstones in riak_core_ring:get_buckets/1](https://github.com/basho/riak_core/pull/495)
* riak_core/497: [Cuttlefish schema for background manager global kill/enable switch](https://github.com/basho/riak_core/pull/497)
* riak_core/498: [Fix a Couple Mixed Cluster Issues w/ Bucket Types](https://github.com/basho/riak_core/pull/498)
* riak_core/499: [properly handle default bucket type in get/1](https://github.com/basho/riak_core/pull/499)
* riak_core/500: [Allow PLT destination to be specified](https://github.com/basho/riak_core/pull/500)
* riak_core/502: [Peer Reviewed Schema Changes](https://github.com/basho/riak_core/pull/502)
* riak_core/503: [Don't discard config options that are not validated](https://github.com/basho/riak_core/pull/503)
* riak_core/504: [fix make test](https://github.com/basho/riak_core/pull/504)
* riak_core/506: [Handle tombstones when accumulating sources](https://github.com/basho/riak_core/pull/506)
* riak_core/507: [Use tools.mk Makefile](https://github.com/basho/riak_core/pull/507)
* riak_core/508: [Made platform_*_dir {level, advanced}](https://github.com/basho/riak_core/pull/508)
* riak_core/510: [Dominates should take timestamp into account.](https://github.com/basho/riak_core/pull/510)
* riak_core/511: [address unmatched_returns in cluster metadata code](https://github.com/basho/riak_core/pull/511)
* riak_core/512: [Add xref target, with exclusions](https://github.com/basho/riak_core/pull/512)
* riak_core/513: [plumb through coverage queries during overload](https://github.com/basho/riak_core/pull/513)
* riak_core/514: [Re-order Resize Transfers so Partitions Not Changing Ownership Go First](https://github.com/basho/riak_core/pull/514)
* riak_core/515: [Made tcpmon aware of ssl sockets for stats.](https://github.com/basho/riak_core/pull/515)
* riak_core/518: [Include directories in generated .conf file.](https://github.com/basho/riak_core/pull/518)
* riak_core/519: [If the security capability is unknown, consider security off](https://github.com/basho/riak_core/pull/519)
* riak_core/520: [add {refresh_iterator, true} for backend folds during handoff send](https://github.com/basho/riak_core/pull/520)
* riak_core/521: ["candidate" misspelled in node replacement error messages](https://github.com/basho/riak_core/pull/521)
* riak_core/523: [Missing whitespace in error message](https://github.com/basho/riak_core/pull/523)
* riak_core/524: [Stop doing implicit role manipulation](https://github.com/basho/riak_core/pull/524)
* riak_core/526: [Cuttlefish RHS Substitutions](https://github.com/basho/riak_core/pull/526)
* riak_core/531: [Removed table manager.](https://github.com/basho/riak_core/pull/531)
* riak_core/534: [Treat users and groups as distinct concepts (extended)](https://github.com/basho/riak_core/pull/534)
* riak_core/536: [Cleanup for permissions assigned to 'all'](https://github.com/basho/riak_core/pull/536)
* riak_core/540: [Fix dialyzer warnings](https://github.com/basho/riak_core/pull/540)
* riak_core/542: [Minor cleanups](https://github.com/basho/riak_core/pull/542)
* riak_core/543: [Add callback annotations to riak_core_vnode](https://github.com/basho/riak_core/pull/543)
* riak_core/544: [Remove those obnoxious debug statements from bucket_fixup_test](https://github.com/basho/riak_core/pull/544)
* riak_core/545: [Update tools.mk to 0.5.3 and add dialyzer ignore file](https://github.com/basho/riak_core/pull/545)
* riak_core/546: [Improve output from (primarily) security commands](https://github.com/basho/riak_core/pull/546)
* riak_core/547: [Switched cuttlefish RHS subs to $ syntax](https://github.com/basho/riak_core/pull/547)
* riak_core/548: [Drop console support for 'all' keyword for global permissions, fix typo](https://github.com/basho/riak_core/pull/548)
* riak_core/549: [Add dialyzer files to gitignore](https://github.com/basho/riak_core/pull/549)
* riak_core/550: [Only wait for the finished message in the updown test](https://github.com/basho/riak_core/pull/550)
* riak_core/551: [Make print-user/group look like print-users/groups](https://github.com/basho/riak_core/pull/551)
* riak_core/555: [Fix overload test](https://github.com/basho/riak_core/pull/555)
* riak_core/562: [Be more rigorous with security command-line arguments](https://github.com/basho/riak_core/pull/562)
* riak_core/566: [Improve riak_ensemble integration](https://github.com/basho/riak_core/pull/566)
* riak_core/567: [Do not allow a bucket type named 'any'](https://github.com/basho/riak_core/pull/567)
* riak_core/568: [Add `dvv_enabled=true` to default props for typed buckets](https://github.com/basho/riak_core/pull/568)
* riak_core/573: [Refactor: rename entry for dot (cos that's what it is)](https://github.com/basho/riak_core/pull/573)
* riak_core/577: [Update core:security:bucket() spec](https://github.com/basho/riak_core/pull/577)
* riak_core/578: [Ensure ensembles reconfigure before nodes exit](https://github.com/basho/riak_core/pull/578)
* riak_core/581: [Correct return type information on riak_core_bucket:set_bucket/2](https://github.com/basho/riak_core/pull/581)
* riak_core/586: [Made many rpc:call/4,5 calls safer if rex is down.](https://github.com/basho/riak_core/pull/586)
* riak_core/587: [Fix hashtree:sha_test_ from timing out](https://github.com/basho/riak_core/pull/587)
* riak_core/589: [Extend hashtree eqc test timeouts](https://github.com/basho/riak_core/pull/589)
* riak_core/591: [Fix minor display bug with security sources](https://github.com/basho/riak_core/pull/591)
* riak_core/592: [change bucket_fixup_test:fixup_test_/0 to wait for ring manager death](https://github.com/basho/riak_core/pull/592)
* riak_core/595: [plug bg_manager_eqc into eunit and address potential race](https://github.com/basho/riak_core/pull/595)
* riak_core/596: [timeout in riak_core_tcp_mon nodeupdown_test_](https://github.com/basho/riak_core/pull/596)
* riak_core/598: [Make riak_core_util:safe_rpc catch exit correctly](https://github.com/basho/riak_core/pull/598)
* riak_core/599: [disable bg manager globally](https://github.com/basho/riak_core/pull/599)
* riak_core/600: [attempt to isolate hashtree tests more by using a reference](https://github.com/basho/riak_core/pull/600)
* riak_core/601: [Add logic to automatically enable consensus system](https://github.com/basho/riak_core/pull/601)
* riak_core/602: [Fix riak_core_util:pmap/2 infinite stall](https://github.com/basho/riak_core/pull/602)
* riak_core/603: [Bugfix/reip update claimant](https://github.com/basho/riak_core/pull/603)
* riak_core/605: [call Mod:handle_overload_info/2 for unknown msgs in vnode_proxy during o...](https://github.com/basho/riak_core/pull/605)
* riak_core/606: [Silence output from core_vnode_eqc and log to file instead](https://github.com/basho/riak_core/pull/606)
* riak_core/609: [Use a proxy process in claimant when joining/removing SC nodes](https://github.com/basho/riak_core/pull/609)
* riak_core/611: [Update tools.mk to v0.5.4](https://github.com/basho/riak_core/pull/611)
* riak_core/612: [Update tools.mk to v0.5.5](https://github.com/basho/riak_core/pull/612)
* riak_jmx/18: [Added cuttlefish schema for riak_jmx](https://github.com/basho/riak_jmx/pull/18)
* riak_jmx/19: [Using newer cuttlefish datatypes](https://github.com/basho/riak_jmx/pull/19)
* riak_jmx/20: [Fix riak_core regression.](https://github.com/basho/riak_jmx/pull/20)
* riak_jmx/21: [Updated JMX schema with newer cuttlefeatures](https://github.com/basho/riak_jmx/pull/21)
* riak_jmx/22: [Add tools.mk and make xref work](https://github.com/basho/riak_jmx/pull/22)
* riak_jmx/23: [{level, advanced} -> hidden](https://github.com/basho/riak_jmx/pull/23)
* riak_jmx/24: [remove useless script](https://github.com/basho/riak_jmx/pull/24)
* riak_jmx/25: [Resolve all outstanding dialyzer warnings.](https://github.com/basho/riak_jmx/pull/25)
* riak_kv/1000: [Update tools.mk to v0.5.5](https://github.com/basho/riak_kv/pull/1000)
* riak_kv/1002: [Update to latest riak_ensemble integrity approach](https://github.com/basho/riak_kv/pull/1002)
* riak_kv/383: [Add disk stats to `/stats` output to make consistent](https://github.com/basho/riak_kv/pull/383)
* riak_kv/561: [Handle worker errors and timeout on 2i reformat](https://github.com/basho/riak_kv/pull/561)
* riak_kv/601: [Move json encoding out of riak_object](https://github.com/basho/riak_kv/pull/601)
* riak_kv/602: [R16B01 compatibility changes.](https://github.com/basho/riak_kv/pull/602)
* riak_kv/603: [Remove -author attributes](https://github.com/basho/riak_kv/pull/603)
* riak_kv/606: [Make the memory backend config behave as documented](https://github.com/basho/riak_kv/pull/606)
* riak_kv/607: [Don't return the timestamp information when folding objects](https://github.com/basho/riak_kv/pull/607)
* riak_kv/608: [fix riak_kv_backend standard tests](https://github.com/basho/riak_kv/pull/608)
* riak_kv/613: [Fix counter protobuf message codes](https://github.com/basho/riak_kv/pull/613)
* riak_kv/639: [Fix HTTP MR error reporting](https://github.com/basho/riak_kv/pull/639)
* riak_kv/643: [Add option for smaller bitcask keys](https://github.com/basho/riak_kv/pull/643)
* riak_kv/647: [Remove duplicate make targets.](https://github.com/basho/riak_kv/pull/647)
* riak_kv/648: [Add dialyzer targets.](https://github.com/basho/riak_kv/pull/648)
* riak_kv/649: [Fix keys fsm EQC test](https://github.com/basho/riak_kv/pull/649)
* riak_kv/651: [Merging 1.4.2 to develop branch](https://github.com/basho/riak_kv/pull/651)
* riak_kv/652: [remove hashtree code and docs](https://github.com/basho/riak_kv/pull/652)
* riak_kv/653: [Add Yokozuna index hook](https://github.com/basho/riak_kv/pull/653)
* riak_kv/654: [Generic AAE Status](https://github.com/basho/riak_kv/pull/654)
* riak_kv/659: [Move riak_core.proto from riak_core.](https://github.com/basho/riak_kv/pull/659)
* riak_kv/661: [Added Mutators to support changing object on read/write](https://github.com/basho/riak_kv/pull/661)
* riak_kv/662: [Add security to Riak](https://github.com/basho/riak_kv/pull/662)
* riak_kv/663: [Modify the version 1 format to support bucket types](https://github.com/basho/riak_kv/pull/663)
* riak_kv/664: [Use yokozuna or riak_search for "search" MapReduce inputs](https://github.com/basho/riak_kv/pull/664)
* riak_kv/668: [Fix rebar.config to use {branch, "name"}](https://github.com/basho/riak_kv/pull/668)
* riak_kv/669: [Add permissions to crdt requests.](https://github.com/basho/riak_kv/pull/669)
* riak_kv/672: [Bump rebar; fix makefile.](https://github.com/basho/riak_kv/pull/672)
* riak_kv/673: [update bucket validator for changes in core](https://github.com/basho/riak_kv/pull/673)
* riak_kv/675: [Set text/plain as content-type for permission errors](https://github.com/basho/riak_kv/pull/675)
* riak_kv/677: [bucket type console functions](https://github.com/basho/riak_kv/pull/677)
* riak_kv/678: [Error reason for put is not propagated back to client](https://github.com/basho/riak_kv/pull/678)
* riak_kv/680: [Add warning/max object limits](https://github.com/basho/riak_kv/pull/680)
* riak_kv/681: [Improve logging so we can tell which vnode failed to start up.](https://github.com/basho/riak_kv/pull/681)
* riak_kv/685: [add option to filter keys that are no longer in preflist during fold](https://github.com/basho/riak_kv/pull/685)
* riak_kv/692: [Fixed call to undefined function in kv_mutator](https://github.com/basho/riak_kv/pull/692)
* riak_kv/694: [Add bucket types support to HTTP.](https://github.com/basho/riak_kv/pull/694)
* riak_kv/695: [refactored in riak_kv bits from riak.schema with tests](https://github.com/basho/riak_kv/pull/695)
* riak_kv/697: [Re-introduce the 1.4 counters API](https://github.com/basho/riak_kv/pull/697)
* riak_kv/698: [Coerce the datatype property into an atom.](https://github.com/basho/riak_kv/pull/698)
* riak_kv/702: [moved default bucket props to riak_core](https://github.com/basho/riak_kv/pull/702)
* riak_kv/703: [Fix MR/bucket types incompatibility](https://github.com/basho/riak_kv/pull/703)
* riak_kv/704: [Adjust guard to tolerate bucket types](https://github.com/basho/riak_kv/pull/704)
* riak_kv/705: [Fix include path.](https://github.com/basho/riak_kv/pull/705)
* riak_kv/707: [Re-add security to the wm_counters endpoint](https://github.com/basho/riak_kv/pull/707)
* riak_kv/708: [Add validator support for data types ](https://github.com/basho/riak_kv/pull/708)
* riak_kv/710: [Support for strongly consistent Riak](https://github.com/basho/riak_kv/pull/710)
* riak_kv/711: [Add hook to Yokozuna for handoff](https://github.com/basho/riak_kv/pull/711)
* riak_kv/712: [Use the deep equality check for riak_objects.](https://github.com/basho/riak_kv/pull/712)
* riak_kv/714: [Correctly enocde the update response when return_body is true](https://github.com/basho/riak_kv/pull/714)
* riak_kv/718: [Key-specific changes for FS2 testing](https://github.com/basho/riak_kv/pull/718)
* riak_kv/719: [Avoid mutator code path when necessary.](https://github.com/basho/riak_kv/pull/719)
* riak_kv/720: [added secure_referer_check to riak_kv schema](https://github.com/basho/riak_kv/pull/720)
* riak_kv/722: [Improve the documentation for the storage_backend setting.](https://github.com/basho/riak_kv/pull/722)
* riak_kv/723: [Fix merge to properly operate over dict.](https://github.com/basho/riak_kv/pull/723)
* riak_kv/725: [Fix type specification.](https://github.com/basho/riak_kv/pull/725)
* riak_kv/727: [moved some schema bits in from riak.schema](https://github.com/basho/riak_kv/pull/727)
* riak_kv/728: [Transform an empty context to a `undefined` for the PB protocol](https://github.com/basho/riak_kv/pull/728)
* riak_kv/729: [Fix consistent_object mixed-mode failure.](https://github.com/basho/riak_kv/pull/729)
* riak_kv/730: [add riak_pb to apps in src/riak_kv.app.src](https://github.com/basho/riak_kv/pull/730)
* riak_kv/731: [remove ebloom from rebar.config](https://github.com/basho/riak_kv/pull/731)
* riak_kv/732: [Add HTTP API for datatypes.](https://github.com/basho/riak_kv/pull/732)
* riak_kv/733: [make riak_kv_test_util app startup less brittle](https://github.com/basho/riak_kv/pull/733)
* riak_kv/734: [Refactor FSMs to do less work](https://github.com/basho/riak_kv/pull/734)
* riak_kv/742: [Pass crdt_op to vnode on read-repair.](https://github.com/basho/riak_kv/pull/742)
* riak_kv/745: [Add CRDT stats](https://github.com/basho/riak_kv/pull/745)
* riak_kv/746: [Use a DVV like approach to stop sibling explosion](https://github.com/basho/riak_kv/pull/746)
* riak_kv/751: [documentation for memory_backend.max_memory](https://github.com/basho/riak_kv/pull/751)
* riak_kv/756: [Clean up two security holes: link walking and arbitrary erlang MR](https://github.com/basho/riak_kv/pull/756)
* riak_kv/757: [New cuttlefish api changes for schemas](https://github.com/basho/riak_kv/pull/757)
* riak_kv/758: [Merge CRDTs inside riak_object merge](https://github.com/basho/riak_kv/pull/758)
* riak_kv/760: [make it possible to disable overload protection](https://github.com/basho/riak_kv/pull/760)
* riak_kv/761: [Change the way we handle removes with a context](https://github.com/basho/riak_kv/pull/761)
* riak_kv/763: [Make vnode_status overload safe](https://github.com/basho/riak_kv/pull/763)
* riak_kv/769: [Integrate Background Manager with Handoff And AAE Tree Rebuilds](https://github.com/basho/riak_kv/pull/769)
* riak_kv/770: [Strong Consistency and Other Riak KV Bucket Validators](https://github.com/basho/riak_kv/pull/770)
* riak_kv/771: [Add support for conditional postcommit hooks](https://github.com/basho/riak_kv/pull/771)
* riak_kv/773: [Remove reset functionality from bucket-type API.](https://github.com/basho/riak_kv/pull/773)
* riak_kv/776: [2i improvements added in 1.4.4-1.4.6](https://github.com/basho/riak_kv/pull/776)
* riak_kv/778: [Fix bug with stats for legacy counters.](https://github.com/basho/riak_kv/pull/778)
* riak_kv/782: [Don't encode object dot for JS map-reduce](https://github.com/basho/riak_kv/pull/782)
* riak_kv/785: [Add timer:sleep()-based throttle to riak_kv_exchange_fsm:read_repair_keydiff()](https://github.com/basho/riak_kv/pull/785)
* riak_kv/787: [Fix test-compile target, skipping forced recompile if EQC is not present.](https://github.com/basho/riak_kv/pull/787)
* riak_kv/788: [fix put fsm's use of random:uniform_s when choosing forwarding node](https://github.com/basho/riak_kv/pull/788)
* riak_kv/789: [Fix logging call](https://github.com/basho/riak_kv/pull/789)
* riak_kv/792: [Unfold the put options at the start](https://github.com/basho/riak_kv/pull/792)
* riak_kv/793: [Cuttlefish schema for background manager subsystem (aae and handoff) kill/enable switch](https://github.com/basho/riak_kv/pull/793)
* riak_kv/794: [Allow PLT destination to be specified](https://github.com/basho/riak_kv/pull/794)
* riak_kv/796: [peer reviewed schema changes... and more](https://github.com/basho/riak_kv/pull/796)
* riak_kv/808: [Use tools.mk in Makefile](https://github.com/basho/riak_kv/pull/808)
* riak_kv/810: [Add "never" atom to options for anti_entropy_expire config.](https://github.com/basho/riak_kv/pull/810)
* riak_kv/814: [Fix DVV merge to handle "skewed dots"](https://github.com/basho/riak_kv/pull/814)
* riak_kv/817: [Bring 1.4.7 2i AAE fixes to 2.0](https://github.com/basho/riak_kv/pull/817)
* riak_kv/820: [Remved mutator system.](https://github.com/basho/riak_kv/pull/820)
* riak_kv/821: [Add xref target, with exclusions](https://github.com/basho/riak_kv/pull/821)
* riak_kv/822: [Ajs overload plumbing](https://github.com/basho/riak_kv/pull/822)
* riak_kv/824: [display default type properties in console status output](https://github.com/basho/riak_kv/pull/824)
* riak_kv/825: [revert allow_mult default only for untyped (default type) buckets](https://github.com/basho/riak_kv/pull/825)
* riak_kv/826: [Made anti_entropy.data_dir appear in default .conf file](https://github.com/basho/riak_kv/pull/826)
* riak_kv/827: [warn about inability to downgrade after activating bucket type](https://github.com/basho/riak_kv/pull/827)
* riak_kv/828: [Add riak_core stats to riak-admin status](https://github.com/basho/riak_kv/pull/828)
* riak_kv/829: [Make sure background and table manager are started for tests that need t.hem](https://github.com/basho/riak_kv/pull/829)
* riak_kv/832: [Replaced mustache template in schema with RHS sub for platform_bin_dir](https://github.com/basho/riak_kv/pull/832)
* riak_kv/833: [Move metadata settings to advanced level.](https://github.com/basho/riak_kv/pull/833)
* riak_kv/835: [Rebased push of riak_kv#807 (put_merge refactor + EQC)](https://github.com/basho/riak_kv/pull/835)
* riak_kv/840: [Export riak_kv_vnode:get/4](https://github.com/basho/riak_kv/pull/840)
* riak_kv/841: [Backwards compat for users running map reduce jobs that use riak_kv_counter](https://github.com/basho/riak_kv/pull/841)
* riak_kv/843: [Remove table manager from tests, as it has been removed from riak core](https://github.com/basho/riak_kv/pull/843)
* riak_kv/848: [Fix incorrect specification.](https://github.com/basho/riak_kv/pull/848)
* riak_kv/849: [Add config entry for riak_dt.binary_compression.](https://github.com/basho/riak_kv/pull/849)
* riak_kv/850: [deprecate riak_kv_backup:backup/3](https://github.com/basho/riak_kv/pull/850)
* riak_kv/851: [Update sysctl checks](https://github.com/basho/riak_kv/pull/851)
* riak_kv/852: [Use iterator_refresh backend option for some folds](https://github.com/basho/riak_kv/pull/852)
* riak_kv/853: [handle iterator_refresh option checking inside riak_kv_vnode only](https://github.com/basho/riak_kv/pull/853)
* riak_kv/855: [Add Yokozuna stats so they appear in console and http /stats](https://github.com/basho/riak_kv/pull/855)
* riak_kv/857: [Make get_put_monitor_eqc pass deterministically](https://github.com/basho/riak_kv/pull/857)
* riak_kv/860: [Fix use of fsm timeouts in 2i AAE](https://github.com/basho/riak_kv/pull/860)
* riak_kv/861: [Object limit changes done in 1.4.8](https://github.com/basho/riak_kv/pull/861)
* riak_kv/862: [Changed cuttlefish rhs sub to $ syntax](https://github.com/basho/riak_kv/pull/862)
* riak_kv/867: [Make get_put_monitor_eqc pass during `make test`](https://github.com/basho/riak_kv/pull/867)
* riak_kv/868: [a couple bucket type console command improvements](https://github.com/basho/riak_kv/pull/868)
* riak_kv/869: [Send reply in event of a premature exit (via throw) from fold.](https://github.com/basho/riak_kv/pull/869)
* riak_kv/871: [Remove full object as context code and use DT's built in contexts](https://github.com/basho/riak_kv/pull/871)
* riak_kv/874: [Fix list_to_binary invocations on permissions errors](https://github.com/basho/riak_kv/pull/874)
* riak_kv/879: [Export AAE format functions for reuse](https://github.com/basho/riak_kv/pull/879)
* riak_kv/881: [Export riak_client type.](https://github.com/basho/riak_kv/pull/881)
* riak_kv/882: [Provide a minimal type for query_def.](https://github.com/basho/riak_kv/pull/882)
* riak_kv/883: [multi_backend.schema doesn't handle error cases of cuttlefish_generator:map/2](https://github.com/basho/riak_kv/pull/883)
* riak_kv/885: [First round of Dialyzer fixes.](https://github.com/basho/riak_kv/pull/885)
* riak_kv/886: [Add yz_stat exception to xref analysis](https://github.com/basho/riak_kv/pull/886)
* riak_kv/887: [Update to work with latest riak_ensemble API](https://github.com/basho/riak_kv/pull/887)
* riak_kv/888: [Fix handling of bucket+type with MR keyfilters](https://github.com/basho/riak_kv/pull/888)
* riak_kv/891: [Make `dvv_enabled` a bucket property](https://github.com/basho/riak_kv/pull/891)
* riak_kv/892: [comapring -> comparing](https://github.com/basho/riak_kv/pull/892)
* riak_kv/896: [Implement AAE-based ensemble syncing](https://github.com/basho/riak_kv/pull/896)
* riak_kv/898: [Bound testing time for riak_object_dvv_statem](https://github.com/basho/riak_kv/pull/898)
* riak_kv/899: [keys_fsm_eqc intermittently fails](https://github.com/basho/riak_kv/pull/899)
* riak_kv/904: [Some small dialyzer progress](https://github.com/basho/riak_kv/pull/904)
* riak_kv/907: [Feature/riak kv pb index dialyzer](https://github.com/basho/riak_kv/pull/907)
* riak_kv/910: [Use vclock encapsulation of dot/pure_dot](https://github.com/basho/riak_kv/pull/910)
* riak_kv/911: [Ignore all current dialyzer errors](https://github.com/basho/riak_kv/pull/911)
* riak_kv/913: [Ignore {ack,_,now_executing} that arrive late](https://github.com/basho/riak_kv/pull/913)
* riak_kv/914: [Clean up webmachine resource specs](https://github.com/basho/riak_kv/pull/914)
* riak_kv/916: [Add sys_monitor_count and sys_port_count to stats.](https://github.com/basho/riak_kv/pull/916)
* riak_kv/920: [riak_core_security:check_permission/2 usage](https://github.com/basho/riak_kv/pull/920)
* riak_kv/923: [kv_vnode calling coverage_filter with 'all'](https://github.com/basho/riak_kv/pull/923)
* riak_kv/924: [Fix some AAE dialyzer errors](https://github.com/basho/riak_kv/pull/924)
* riak_kv/925: [Add Bitcask tombstone2 upgrade procedure](https://github.com/basho/riak_kv/pull/925)
* riak_kv/926: [Ensure nodes don't transition to exiting too soon](https://github.com/basho/riak_kv/pull/926)
* riak_kv/931: [Specified return code for riak_kv_console:bucket_type_status](https://github.com/basho/riak_kv/pull/931)
* riak_kv/932: [fix #929: wait for riak_kv_stat to unregister](https://github.com/basho/riak_kv/pull/932)
* riak_kv/933: [Use binary_to_atom/2 instead of binary_to_existing_atom/2.](https://github.com/basho/riak_kv/pull/933)
* riak_kv/935: [Fix limit checks in riak_kv_env for R16B02-basho5](https://github.com/basho/riak_kv/pull/935)
* riak_kv/938: [fix #936: handle slow metrics for get_put_monitor_eqc](https://github.com/basho/riak_kv/pull/938)
* riak_kv/939: [fix #937: spawn tests to avoid unrelated messages](https://github.com/basho/riak_kv/pull/939)
* riak_kv/941: [Fix dialyzer error in kv_mrc_map](https://github.com/basho/riak_kv/pull/941)
* riak_kv/942: [Better error handling from basho/riak_kv#935](https://github.com/basho/riak_kv/pull/942)
* riak_kv/943: [Optimize riak_kv_entropy_info:exchanges/2](https://github.com/basho/riak_kv/pull/943)
* riak_kv/945: [Comment out get_put_monitor_eqc property](https://github.com/basho/riak_kv/pull/945)
* riak_kv/946: [Bound eqc testing time](https://github.com/basho/riak_kv/pull/946)
* riak_kv/947: [Update repair count type specs in riak_kv_entropy_info](https://github.com/basho/riak_kv/pull/947)
* riak_kv/950: [Fix two places where the bucket-type was hard-coded "default" in securit...](https://github.com/basho/riak_kv/pull/950)
* riak_kv/951: [Use the embedded counter inside the map](https://github.com/basho/riak_kv/pull/951)
* riak_kv/952: [Fix security-enabled delete handling for nonexistent resources.](https://github.com/basho/riak_kv/pull/952)
* riak_kv/953: [Add basic Erlang-VM-style tracing utilities](https://github.com/basho/riak_kv/pull/953)
* riak_kv/956: [Cleaner output for bucket type creation errors](https://github.com/basho/riak_kv/pull/956)
* riak_kv/963: [forward port membackend fixes](https://github.com/basho/riak_kv/pull/963)
* riak_kv/964: [disable bg manager integration w/ handoff & AAE](https://github.com/basho/riak_kv/pull/964)
* riak_kv/968: [Expose a 2-arity from_mod function for embedded mod map](https://github.com/basho/riak_kv/pull/968)
* riak_kv/970: [Verify bucket type existence for list keys and buckets operations](https://github.com/basho/riak_kv/pull/970)
* riak_kv/971: [Improve on 5 min AAE+ownership change stall](https://github.com/basho/riak_kv/pull/971)
* riak_kv/972: [Rename security permission from riak_search.query to search.query](https://github.com/basho/riak_kv/pull/972)
* riak_kv/973: [Resolves GH #859 by setting tmpdir for sorting AAE disk logs.](https://github.com/basho/riak_kv/pull/973)
* riak_kv/974: [Fix 'undefined' bucket type issue.](https://github.com/basho/riak_kv/pull/974)
* riak_kv/976: [Fake Bitcask version for tests](https://github.com/basho/riak_kv/pull/976)
* riak_kv/977: [Fix reip loading of riak_core](https://github.com/basho/riak_kv/pull/977)
* riak_kv/978: [Verify strong consistency configuration on startup](https://github.com/basho/riak_kv/pull/978)
* riak_kv/979: [Add console commands to inspect ensemble system](https://github.com/basho/riak_kv/pull/979)
* riak_kv/980: [Make ensemble trust/paranoia configurable](https://github.com/basho/riak_kv/pull/980)
* riak_kv/981: [Add basic stats for strongly consistent operations](https://github.com/basho/riak_kv/pull/981)
* riak_kv/987: [Fix MOD_MAP when datatype update request includes return_body=true](https://github.com/basho/riak_kv/pull/987)
* riak_kv/989: [SC ensemble peers should wait for riak_kv service](https://github.com/basho/riak_kv/pull/989)
* riak_kv/990: [Add riak_kv_ensemble_backend:handle_down/4](https://github.com/basho/riak_kv/pull/990)
* riak_kv/992: [Correct discrepancy between do_get_term and case clause in do_delete](https://github.com/basho/riak_kv/pull/992)
* riak_kv/993: [Fix AAE on/off detection](https://github.com/basho/riak_kv/pull/993)
* riak_kv/994: [add riak_kv_vnode:handle_overload_info/2](https://github.com/basho/riak_kv/pull/994)
* riak_kv/997: [Ignore all warnings in generated riak_core_pb](https://github.com/basho/riak_kv/pull/997)
* riak_kv/999: [Update tools.mk to v0.5.4](https://github.com/basho/riak_kv/pull/999)
* riak_pb/53: [Add yokozuna index and schema admin messages](https://github.com/basho/riak_pb/pull/53)
* riak_pb/54: [Bucket type support](https://github.com/basho/riak_pb/pull/54)
* riak_pb/55: [Add security to Riak](https://github.com/basho/riak_pb/pull/55)
* riak_pb/56: [Add yokozuna messages to python](https://github.com/basho/riak_pb/pull/56)
* riak_pb/58: [Add term_regex to 2i query](https://github.com/basho/riak_pb/pull/58)
* riak_pb/59: [Remove bucket type field from counter messages, unsupported.](https://github.com/basho/riak_pb/pull/59)
* riak_pb/61: [Add datatype bucket property (read-only)](https://github.com/basho/riak_pb/pull/61)
* riak_pb/63: [Generate an Erlang module from a CSV of message/code mappings.](https://github.com/basho/riak_pb/pull/63)
* riak_pb/64: [An empty LWW register has the value `undefined` which is not binary()](https://github.com/basho/riak_pb/pull/64)
* riak_pb/65: [Fix bug when decoding map values in an update response that had return_body=true.](https://github.com/basho/riak_pb/pull/65)
* riak_pb/67: [Renaming yz_index to search_index](https://github.com/basho/riak_pb/pull/67)
* riak_pb/69: [Upgrade java protobuf version from 2.4.1 to 2.5.0](https://github.com/basho/riak_pb/pull/69)
* riak_pb/70: [Inital pass at generating Protocol Buffer headers/source for C](https://github.com/basho/riak_pb/pull/70)
* riak_pb/74: [Remove reset functionality from bucket-type API.](https://github.com/basho/riak_pb/pull/74)
* riak_pb/75: [Fix typo in riak_pb_messages.csv](https://github.com/basho/riak_pb/pull/75)
* riak_pb/77: [Generate Python from message code mappings.](https://github.com/basho/riak_pb/pull/77)
* riak_pb/79: [Require GPG/PGP signing of the python release.](https://github.com/basho/riak_pb/pull/79)
* riak_pb/80: [Add n_val to Yokozuna Index](https://github.com/basho/riak_pb/pull/80)
* riak_pb/82: [Maven will autogenerate RiakMessageCodes](https://github.com/basho/riak_pb/pull/82)
* riak_pb/84: [Resolves issue with maven >= 3.1.x](https://github.com/basho/riak_pb/pull/84)
* riak_pb/85: [Relax the protobuf version restriction, allowing 2.5.0.](https://github.com/basho/riak_pb/pull/85)
* riak_pb/86: [Add missing 'consistent' bucket(-type) property.](https://github.com/basho/riak_pb/pull/86)
* riak_pb/87: [Convert Erlang build steps to tools.mk.](https://github.com/basho/riak_pb/pull/87)
* riak_pb/89: [compile in 17.0: clients are compiles with various OTP versions](https://github.com/basho/riak_pb/pull/89)
* riak_pb/91: [Clean up dialyzer specs for context/values](https://github.com/basho/riak_pb/pull/91)
* riak_pb/93: [Remove the 'add field' operation from riak_dt maps](https://github.com/basho/riak_pb/pull/93)
* riak_pb/94: [Fix dialyzer and xref warnings](https://github.com/basho/riak_pb/pull/94)
* riak_pipe/80: [catch and upgrade riak_core_fold_req_v1](https://github.com/basho/riak_pipe/pull/80)
* riak_pipe/83: [Use tools.mk in Makefile](https://github.com/basho/riak_pipe/pull/83)
* riak_pipe/84: [Add xref target, with exclusions](https://github.com/basho/riak_pipe/pull/84)
* riak_pipe/86: [rpc:call try/catch rex](https://github.com/basho/riak_pipe/pull/86)
* riak_pipe/87: [Resolve dialyzer warnings.](https://github.com/basho/riak_pipe/pull/87)
* riak_repl/318: [fixed typo in debug message, not urgent](https://github.com/basho/riak_repl/pull/318)
* riak_repl/324: [removed delayed_write option to file:open](https://github.com/basho/riak_repl/pull/324)
* riak_repl/326: [Cache the inet:peername for better error condition logging. ](https://github.com/basho/riak_repl/pull/326)
* riak_repl/332: [Make the interval the rtsink rechecks the active flag configurable. ](https://github.com/basho/riak_repl/pull/332)
* riak_repl/356: [Use {active, once} for the rt_sink socket.](https://github.com/basho/riak_repl/pull/356)
* riak_repl/357: [don't blow up on stat errors](https://github.com/basho/riak_repl/pull/357)
* riak_repl/371: [Catch errors on connect in the RT source connection](https://github.com/basho/riak_repl/pull/371)
* riak_repl/372: [Add overload check and recovery to realtime queue](https://github.com/basho/riak_repl/pull/372)
* riak_repl/388: [merge 1.4 changes to develop](https://github.com/basho/riak_repl/pull/388)
* riak_repl/391: [Fix for heartbeat timeout bug discovered in riak_test repl_rt_heartbeat](https://github.com/basho/riak_repl/pull/391)
* riak_repl/396: [Repl schema for cuttlefish](https://github.com/basho/riak_repl/pull/396)
* riak_repl/399: [Handle decommissioned clusterid for replicated CS manifests](https://github.com/basho/riak_repl/pull/399)
* riak_repl/400: [Fixed realtime protocol version check](https://github.com/basho/riak_repl/pull/400)
* riak_repl/402: [Reduced Replication](https://github.com/basho/riak_repl/pull/402)
* riak_repl/407: [Add makefile with dialyzer targets.](https://github.com/basho/riak_repl/pull/407)
* riak_repl/408: [Fixed repl mutator registration](https://github.com/basho/riak_repl/pull/408)
* riak_repl/412: [Fix for Issue 384: Repl stats errors](https://github.com/basho/riak_repl/pull/412)
* riak_repl/413: [Merge 1.4 changes into develop, part 2](https://github.com/basho/riak_repl/pull/413)
* riak_repl/418: [merge repl hook fix from 1.4](https://github.com/basho/riak_repl/pull/418)
* riak_repl/422: [Fix for RTQ EQC prop_main and prop_parallel](https://github.com/basho/riak_repl/pull/422)
* riak_repl/435: [Handle race by treating cluster as disconnected.](https://github.com/basho/riak_repl/pull/435)
* riak_repl/437: [fixed misleading typo in connection msg](https://github.com/basho/riak_repl/pull/437)
* riak_repl/438: [Feature/mw/rtq percentage used](https://github.com/basho/riak_repl/pull/438)
* riak_repl/439: [add rebar.config dep for ebloom](https://github.com/basho/riak_repl/pull/439)
* riak_repl/453: [Use loopback interface flag rather than interface name.](https://github.com/basho/riak_repl/pull/453)
* riak_repl/455: [Remove undeliverables from the real time queue.](https://github.com/basho/riak_repl/pull/455)
* riak_repl/456: [Switch fail_on_warning to warnings_as_errors.](https://github.com/basho/riak_repl/pull/456)
* riak_repl/457: [Compilation changes](https://github.com/basho/riak_repl/pull/457)
* riak_repl/463: [Bugfix/dp/pg proxy leader changes develop](https://github.com/basho/riak_repl/pull/463)
* riak_repl/465: [Resolve race condition when joining a node.](https://github.com/basho/riak_repl/pull/465)
* riak_repl/467: [Prevent potential race condition.](https://github.com/basho/riak_repl/pull/467)
* riak_repl/478: [Port connection manager changes from 1.4 to 2.0](https://github.com/basho/riak_repl/pull/478)
* riak_repl/479: [Add nicer error message for service manager bind error.](https://github.com/basho/riak_repl/pull/479)
* riak_repl/480: [bring additional 1.4 changes to develop for 2.0](https://github.com/basho/riak_repl/pull/480)
* riak_repl/481: [Fix compilation errors in EQC tests.](https://github.com/basho/riak_repl/pull/481)
* riak_repl/482: [Avoid arithmetic on folsom error tuples](https://github.com/basho/riak_repl/pull/482)
* riak_repl/484: [Cuttlefish api changes and new unit test](https://github.com/basho/riak_repl/pull/484)
* riak_repl/487: [added a v2 deprecation lager:warning](https://github.com/basho/riak_repl/pull/487)
* riak_repl/488: [Integrate fullsync with the background manager](https://github.com/basho/riak_repl/pull/488)
* riak_repl/491: [official deprecation notice from prod mgmt](https://github.com/basho/riak_repl/pull/491)
* riak_repl/501: [Port of 1.4 AAE replication fixes to 2.0.](https://github.com/basho/riak_repl/pull/501)
* riak_repl/503: [Don't write empty clusters to the ring, ever](https://github.com/basho/riak_repl/pull/503)
* riak_repl/505: [Fix and refactor EQC, Eunit tests](https://github.com/basho/riak_repl/pull/505)
* riak_repl/506: [fixes for riak_ee #159](https://github.com/basho/riak_repl/pull/506)
* riak_repl/509: [Use tools.mk in Makefile](https://github.com/basho/riak_repl/pull/509)
* riak_repl/511: [Ensure we update the bloom for object differences.](https://github.com/basho/riak_repl/pull/511)
* riak_repl/514: [Prevent deadlock in hashtree compare.](https://github.com/basho/riak_repl/pull/514)
* riak_repl/516: [Removed reduced repl](https://github.com/basho/riak_repl/pull/516)
* riak_repl/517: [Add xref and remove ancient couchDB cruft](https://github.com/basho/riak_repl/pull/517)
* riak_repl/520: [Add protocol version checks in typed-bucket handling to fullsync for backward compatibility](https://github.com/basho/riak_repl/pull/520)
* riak_repl/521: [Use iterator_refresh option during vnode folds](https://github.com/basho/riak_repl/pull/521)
* riak_repl/535: [Make the heartbeat times in seconds, like it says in the docs](https://github.com/basho/riak_repl/pull/535)
* riak_repl/536: [Use riak_core_vnode_master:command_return_vnode when doing vnode folds](https://github.com/basho/riak_repl/pull/536)
* riak_repl/538: [Normalize representation.](https://github.com/basho/riak_repl/pull/538)
* riak_repl/539: [Remove some more couch_merkle holdovers](https://github.com/basho/riak_repl/pull/539)
* riak_repl/541: [Minor dialyzer fixes.](https://github.com/basho/riak_repl/pull/541)
* riak_repl/546: [fixing casacding/cascading and realtime_recv_kgbps typos](https://github.com/basho/riak_repl/pull/546)
* riak_repl/550: [Couple of simple bugs noticed while reading code](https://github.com/basho/riak_repl/pull/550)
* riak_repl/551: [Dialyzer fixes.](https://github.com/basho/riak_repl/pull/551)
* riak_repl/552: [More dialyzer fixes.](https://github.com/basho/riak_repl/pull/552)
* riak_repl/554: [Add fullsync stat caching on a configurable interval.](https://github.com/basho/riak_repl/pull/554)
* riak_repl/558: [Fixes wm stats for multiple client connections](https://github.com/basho/riak_repl/pull/558)
* riak_repl/559: [Bloom serialization.](https://github.com/basho/riak_repl/pull/559)
* riak_repl/562: [Bug/mw/correctly jsonify mutiple fs source sink](https://github.com/basho/riak_repl/pull/562)
* riak_repl/563: [Logging cleanup.](https://github.com/basho/riak_repl/pull/563)
* riak_repl/564: [Another round of Dialyzer work.](https://github.com/basho/riak_repl/pull/564)
* riak_repl/566: [Finish him.](https://github.com/basho/riak_repl/pull/566)
* riak_repl/567: [Use valid API for starting strategy modules in riak_repl_tcp_server](https://github.com/basho/riak_repl/pull/567)
* riak_repl/568: [Ignore unknown functions.](https://github.com/basho/riak_repl/pull/568)
* riak_repl/569: [Add callback annotations to gen_leader.](https://github.com/basho/riak_repl/pull/569)
* riak_repl/571: [Fix source/sink typo.](https://github.com/basho/riak_repl/pull/571)
* riak_repl/578: [Fixes monitor leak for realtime objects (#204).](https://github.com/basho/riak_repl/pull/578)
* riak_repl/584: [Fix JSONifying datetimes in the webmachine stats](https://github.com/basho/riak_repl/pull/584)
* riak_repl/587: [Bugfix/mw/safer rpc](https://github.com/basho/riak_repl/pull/587)
* riak_repl/589: [Update strip_postcommit to remove all possible repl hooks.](https://github.com/basho/riak_repl/pull/589)
* riak_repl/590: [Allow retries when location_down.](https://github.com/basho/riak_repl/pull/590)
* riak_repl/591: [Ensure that cancel_fullsync works.](https://github.com/basho/riak_repl/pull/591)
* riak_repl/592: [No longer use sync_send_event.](https://github.com/basho/riak_repl/pull/592)
* riak_repl/595: [Disable background manager by default.](https://github.com/basho/riak_repl/pull/595)
* riak_repl/596: [Don't attempt to send heartbeat when invalid.](https://github.com/basho/riak_repl/pull/596)
* riak_repl/598: [Add logic to clear cancelled connection requests](https://github.com/basho/riak_repl/pull/598)
* riak_repl/599: [Race condition causes fullsync replication to never complete](https://github.com/basho/riak_repl/pull/599)
* riak_repl/601: [Handle synchronous events for rolling upgrades.](https://github.com/basho/riak_repl/pull/601)
* riak_repl/604: [Change lager:notice in trim_q_entries/4 to lager:debug](https://github.com/basho/riak_repl/pull/604)
* riak_repl_pb_api/10: [Makefile and repo cleanup](https://github.com/basho/riak_repl_pb_api/pull/10)
* riak_repl_pb_api/11: [Don't crash when {error, disconnected}.](https://github.com/basho/riak_repl_pb_api/pull/11)
* riak_repl_pb_api/4: [be dialyzer friendly at riakc_pb_socket:tunnel/4](https://github.com/basho/riak_repl_pb_api/pull/4)
* riak_repl_pb_api/5: [Switch back to develop branch.](https://github.com/basho/riak_repl_pb_api/pull/5)
* riak_repl_pb_api/9: [bumped riak_pb to 2.0.0.11](https://github.com/basho/riak_repl_pb_api/pull/9)
* riak_search/138: [use tuple modules instead of parameterized modules](https://github.com/basho/riak_search/pull/138)
* riak_search/150: [Update riak_search to support some new core apis](https://github.com/basho/riak_search/pull/150)
* riak_search/151: [don't forward folds in handle_handoff_command; do upgrade properly in ha.ndle_command](https://github.com/basho/riak_search/pull/151)
* riak_search/153: [added {enabled, false} to app.src](https://github.com/basho/riak_search/pull/153)
* riak_search/154: [Fix search tests](https://github.com/basho/riak_search/pull/154)
* riak_search/156: [Don't start riak_search is security is enabled](https://github.com/basho/riak_search/pull/156)
* riak_search/158: [Allow search to start if security is enabled, just disable its APIs](https://github.com/basho/riak_search/pull/158)
* riak_search/160: [Add deprecation notice on Riak Search startup](https://github.com/basho/riak_search/pull/160)
* riak_snmp/10: [fix unit tests for Erlang R16B01](https://github.com/basho/riak_snmp/pull/10)
* riak_snmp/11: [look for mib_dir in riak_snmp not riak](https://github.com/basho/riak_snmp/pull/11)
* riak_snmp/12: [fix "make clean" and .gitignore](https://github.com/basho/riak_snmp/pull/12)
* riak_snmp/13: [fix error due to missing SNMP directory](https://github.com/basho/riak_snmp/pull/13)
* riak_snmp/14: [fix unit test failures caused by missing module](https://github.com/basho/riak_snmp/pull/14)
* riak_snmp/15: [Changes for new cuttlefish api](https://github.com/basho/riak_snmp/pull/15)
* riak_snmp/16: [Confbal changes to cuttlefish schema.](https://github.com/basho/riak_snmp/pull/16)
* riak_snmp/17: [Add tools.mk and xref](https://github.com/basho/riak_snmp/pull/17)
* riak_snmp/18: [{level, advanced} -> hidden](https://github.com/basho/riak_snmp/pull/18)
* riak_snmp/8: [Add try/catch around poll_stats() call, fix riak_test intermittent failures...](https://github.com/basho/riak_snmp/pull/8)
* riak_snmp/9: [added cuttlefish schema for snmp](https://github.com/basho/riak_snmp/pull/9)
* riak_sysmon/10: [moved riak_sysmon bits of cuttlefish schema here, with tests](https://github.com/basho/riak_sysmon/pull/10)
* riak_sysmon/12: [Added erlang emacs mode comment](https://github.com/basho/riak_sysmon/pull/12)
* riak_sysmon/13: [Update cuttlefish schema.](https://github.com/basho/riak_sysmon/pull/13)
* riak_sysmon/14: [{level, advanced} -> hidden](https://github.com/basho/riak_sysmon/pull/14)
* riaknostic/74: [pull app.config and vm.args from init:get_arguments](https://github.com/basho/riaknostic/pull/74)
* riaknostic/75: [added extra -vm_args to CONFIG_ARGS for easy access by erlang vm](https://github.com/basho/riaknostic/pull/75)
* sidejob/5: [Add tools.mk and a Makefile that uses it](https://github.com/basho/sidejob/pull/5)
* sidejob/6: [Fix tests](https://github.com/basho/sidejob/pull/6)
* sidejob/7: [Update tools.mk, add xref](https://github.com/basho/sidejob/pull/7)
* sidejob/9: [Fix dialyzer warnings.](https://github.com/basho/sidejob/pull/9)
* webmachine/166: [fix unit tests for Erlang/OTP R16](https://github.com/basho/webmachine/pull/166)

----
[1] http://doi.acm.org/10.1145/2332432.2332497
Nuno Preguiça, Carlos Bauqero, Paulo Sérgio Almeida, Victor Fonte, and
Ricardo Gonçalves. 2012. Brief announcement: efficient causality
tracking in  distributed storage systems with dotted version vectors. In
Proceedings of the 2012 ACM symposium on Principles of distributed
computing (PODC '12).
