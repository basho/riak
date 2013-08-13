# Riak 1.4.1 リリースノート

これはバグフィックスのリリースです。主にセカンダリーインデックス、Riak Control、LevelDB に関する修正が含まれています。

* [riak_kv/615](https://github.com/basho/riak_kv/pull/615) で、完全一致のクエリに対するページネーションの問題が修正されました。

* [riak_kv/616](https://github.com/basho/riak_kv/pull/616) 2iクエリでもタイムアウトを設定できるようになりました。

* [riak_kv/618](https://github.com/basho/riak_kv/pull/618)  2iをMapReduceの入力として渡せなかった問題が解決しました。

* [riak_control/120](https://github.com/basho/riak_control/pull/120) 1.3系で動作するクラスタに1.4.0のノードを追加すると Riak Control がRiakノードをクラッシュさせる問題を解決しました。

*  [leveldb/88](https://github.com/basho/leveldb/pull/88) Basho 版の LevelDB の fadvise サポートが改善され、書き込み時の競合がいくつか解決されました。

## Issues / PR's Resolved

* riak_core/351: [Fix catch pattern to match all errors](https://github.com/basho/riak_core/pull/351)
* riak_core/352: [Fix TCP mon to correctly spot nodes coming up](https://github.com/basho/riak_core/pull/352)
* riak_kv/615: [Do not set the start_term to the last seen key for eq (2i)](https://github.com/basho/riak_kv/pull/615)
* riak_kv/616: [Add millisecond timeout parameter to API 2i endpoints](https://github.com/basho/riak_kv/pull/616)
* riak_kv/618: [Strip index term from result before passing to MR (2i)](https://github.com/basho/riak_kv/pull/618)
* riak-erlang-client/108: [Add timeouts to 2i queries](https://github.com/basho/riak-erlang-client/pull/108)
* riak_pb/50: [Add timeout field to 2i messages](https://github.com/basho/riak_pb/pull/50)
* riak_control/120: [Handle incompatible records between the 1.3 and 1.4 release](https://github.com/basho/riak_control/pull/120)
* leveldb/88: [More effective fadvise calls + fix write race conditions](https://github.com/basho/leveldb/pull/88)
* node_package/75: [In RPMs: Do not error on post install script if usermod fails](https://github.com/basho/node_package/pull/75)
* node_package/76: [Fix `riak version` for RPM packages](https://github.com/basho/node_package/pull/76)

# Riak 1.4.0 リリースノート

## 主な新機能／改善点

### バイナリフォーマットの改善 (Improved Binary Format)

Riakに保存されるデータのフォーマットがさらにコンパクトになりました。
新しいフォーマットはデータ保存時のオーバーヘッドを減らしています。
特に小さいオブジェクトや大きなバケット名、キー名、メタデータに対して効果を発揮します。

Riak1.4で新規に作ったRiakクラスターはデフォルトで新しいフォーマットが有効になっています。
Riak1.4へアップグレードする際は、アップグレードをおこなうと新しいフォーマットが有効になります。
新旧のフォーマットは双方共にサポートされますので、アップグレードに伴う追加のオペレーションは必要ありません。

どのフォーマットが用いられるかは `app.config` の `riak_kv` セクションで、`object_format` を設定します。
設定値は `v0`、`v1` のどちらかであり、新しいフォーマットは`v1`です。

この新しいフォーマットはクラスターがサポートすればhandoff でも使用されます。

Riak1.4へアップグレードして新しいフォーマットを有効にしたユーザーが前バージョンへのダウングレードするには、
新しいフォーマットになっているデータを書きかえる必要があります（以前のバージョンのRiakは新しいフォーマットを読み込めません）。
`riak-admin` ではこのユーティリティが用意されています:

```
riak-admin downgrade-objects <kill-handoffs> [<concurrency>]
```

このユーティリティはダウングレードするにあたって、ノードごとに実行します。
`<kill-handoffs>`は `true` か `false` のどちらかの値をとります。`false`の場合、
再フォーマットの前に実行中のhandoffを待ちます。`true`であればノードが受信もしくは
送信している実行中のhandoffは全てkillされます。
再フォーマット中やその後はtransfer-limitが0に設定されます。
`<concurrency>`オプションは1以上の整数値を取ります。これはそのノード上で、
いくつのパーティションが並列で再フォーマットされるかを指定するものです。
デフォルトは２になっています。そしてクラスタ全体がダウングレードされることを見越して、
`downgrade-objects`は推奨フォーマットを `v0` に設定します。`downgrade-objects`はエラーや、
ノードがクラッシュした場合に複数回実行できます。


### `riak attach`の挙動を変更 (Changed behavior of `riak attach`)

`riak attach`にすでに慣れているユーザーは1.4での変更は注目に値します。
`riak attach`は稼働中のerlangノードと通信するためにerlangに提供されている
named pipeを使用していました。この機能は素晴らしいのですが、偶然にも Ctrl-C を押してしまうと
アタッチしたセッションと共に動作中のノードも終了させてしまいます。この振る舞いを今回、
`-remsh` (remote shell)を使ったノード通信に変更しました。これはCtrl-Cが稼働中のノードをkillしないので、より安全です。
分散erlangの問題にぶつかったり、-remsh が望ましくない場合は、以前の`riak attach`と同じ振る舞いする新しいコマンドの
 `riak attach-direct` を使ってください。

### `riak-admin transfers` 改善 (`riak-admin transfers` Improvements)

`riak-admin transfers` でトランスファーごとの進捗を見られるようになりました。
また長いノード名の表示も改善しています。

進捗が表示されるかどうか、またどのように計算されるかはクラスターのバックエンドに依存します。
進捗は `riak_kv_bitcask_backend` と `riak_kv_eleveldb_backend` 、`riak_kv_memory_backend` で有効です。
 `riak_kv_multi_backend` を使うクラスタは進捗の表示が有効になりません。
`riak_kv_bitcask_backend` と `riak_kv_memory_backend` ではキーの総数と送信済み数により進捗が決まります。
バリューサイズが大きく変わる場合には進捗が素直に出ないことがあります。 `riak_kv_eleveldb_backend` では、
進捗は保存されたバイトで測られます。総使用バイトは時に過大評価となるため、表示される進捗は、
実際の進捗を正しく表していることもありますが、最悪の場合には表示よりも先に進んでいる可能性があります。

### Lager アップグレード 1.2.2 to 2.0.0 (Lager Upgrade 1.2.2 to 2.0.0)

Lager は Riak 1.3.x 系で用いられていた 1.2.2 から 2.0.0 へアップデートされました。 Lager 自体の新しい機能は https://github.com/basho/lager をご覧ください。

### クエリ (Querying)

#### 2i ページネーション (Pagination Support in 2i)

セカンダリーインデックス (2i) を拡張し、ページネーションを可能にしました。 `max_results` オプションを指定することで、プロトコルバッファーと HTTP の両方で利用できます。完全な詳細は [こちら](https://github.com/basho/riak_kv/pull/540) をご覧ください。

### クライアント API (Client APIs)

#### クライアント指定のタイムアウト (Client-specified timeouts)

クライアントタイムアウト値をミリ秒で指定できるようになりました。これはオブジェクトの取得、保存、削除に対するデフォルトの内部的なタイムアウトを上書きします。

#### プロトコルバッファーでのバケットプロパティー(Protocol Buffers bucket properties)

プロトコルバッファーは既知のすべてのバケットプロパティーをサポートします。またリセットも可能になりました。

#### バケットリストのストリーミング (List-buckets streaming)

キーリストと同様に、バケットリストもストリーミング可能になりました。
Riak が各ノードからの応答をすべて集めてからクライアントに応答するのではなく、各ノードからの応答に応じてクライアントに送信します。

#### プロトコルバッファーの複数インターフェースサポート (Protocol buffers binds to multiple interfaces)

HTTP と同様に、プロトコルバッファーも複数のインターフェースにバインド出来るようになりました。設定は `pb_port` と `pb_ip` から変更され、単一の `pb` となりました。そこでは IP とポートのリストを指定できます。

### データタイプ (Data Types)

#### PN-カウンター (PN-Counters)

Riak 1.4 では、Riak で初めてとなる分散データ型である PN-カウンターを追加しました。PN-カウンターはインクリメント(P)とデクリメント(N)の両方が可能です。完全な詳細は [こちら](https://github.com/basho/riak_kv/pull/536) をご覧ください。私達は Riak の将来のデータ型を説明するため [CRDT Cookbook](https://github.com/lenary/riak_crdt_cookbook) にも取り組んでいます。

### Riak Control

Riak Control はクラスタとスタンドアローンノードの管理を改善し、変更のステージングとコミットをサポートします。

### 新しいコマンド: riak-debug (New command: riak-debug)

コマンド `riak-debug` はトラブルシューティングのための情報収集自動化の
助けとなるシェルスクリプトです。OS コマンド、Riak コマンド、Riak 設定ファイル、
Riak ログファイルの情報を集めます。スクリプトの使い方やワークフローへの組み込み方は
`riak-debug -h` や `man riak-debug` を参照してください。

### パッケージ/ランタイムの変更 (Packaging / Runtime changes)

Riak 1.4 は [node_package](http://github.com/basho/node_package) を
用いてパッケージ化されるようになりました。Riak CS は初めてのリリースから
このツールを用いています。
この共通化により、一貫性のある機能を提供し、またパッケージング品質を向上させています。
このリリースに対するパッケージングのバグフィックスは "Issues" セクションの
node\_package の項目をご覧ください。

##### 追加、削除されたプラットフォーム (Platforms Added / Removed)

Debian Wheezy と SmartOS 13.1 が 1.4 でサポートされます。
予定通り、 32 bit パッケージは削除されました。

##### パッケージとランタイムの主要な変更点 (Major changes in packages and runtime)

   * Deb と RPM システム向けの init.d スクリプトが、これらのディストリビューションの
  標準に合わせて書き換えられました。
  特に、 init スクリプトは失敗するとノンゼロの終了コードを返します。
  これは他のツールとシームレスに動作するために大きな問題となっていました。
   * start, stop, status コマンドは stdout を読まなくても、戻り値を
  返すようになりました。
  これは私達がずっと抱えていた主要な "技術的負債" で、ようやく修正されました。
   * `riak start/stop` コマンドから `riak.pid` ファイルが作成、削除
  されるようになりました。他のツールは、riak スクリプトや nodetool を知らなくても
  .pid ファイルを活用できます。
   * `riak attach` と `riak attach-direct` はユーザに q() と CTRL-C の意味について
  警告するようになりました。
   * `riak` スクリプトはどのコマンドが riak ユーザ(または root ユーザ)で
  実行されるべきかを明確にしました。
  `getpid` や `ping` のステータスコマンドはどのユーザでも実行できますが、
  `start`, `stop` のようなデーモンコマンドは riak ユーザにより実行されていない
  場合にはエラーとなります。

## Issues / PR's Resolved

* bear/1: [Remove native flag and add kernel,stdlib to app deps](https://github.com/basho/bear/pull/1)
* bitcask/89: [add dialyzer targets](https://github.com/basho/bitcask/issues/89)
* bitcask/92: [Fix merge logging bug introduced by bs-merge-expiration-change branch](https://github.com/basho/bitcask/issues/92)
* folsom/2: [Improve performance of slide histogram](https://github.com/basho/folsom/issues/2)
* leveldb/73: [level work1](https://github.com/basho/leveldb/issues/73)
* leveldb/74: [Add status query for total bytes used by a LevelDB instance](https://github.com/basho/leveldb/issues/74)
* leveldb/75: [Merge of Google 1.6, 1.7, 1.8, and 1.9 releases](https://github.com/basho/leveldb/issues/75)
* leveldb/78: [Repair updated for edge case created with new directory structure.](https://github.com/basho/leveldb/issues/78)
* leveldb/79: [filecache tuning2](https://github.com/basho/leveldb/issues/79)
* leveldb/81: [bloom size limit](https://github.com/basho/leveldb/issues/81)
* leveldb/84: [level work3, change from 3 overlapped levels to 2](https://github.com/basho/leveldb/issues/84)
* merge_index/30: [Remove delayed_write option](https://github.com/basho/merge_index/pull/30)
* mochiweb/7: [Range header fix](https://github.com/basho/mochiweb/issues/7)
* mochiweb/8: [Remove parameterized modules.](https://github.com/basho/mochiweb/issues/8)
* node_package/40: [init script returns success even if riak does not start](https://github.com/basho/node_package/issues/40)
* node_package/43: [Add SRPMS and make RPM version field fully compatible](https://github.com/basho/node_package/issues/43)
* node_package/44: [Convert RPM init script to fall in line with Redhat style](https://github.com/basho/node_package/issues/44)
* node_package/47: [Add app_epath.sh, a POSIX app.config parsing utility.](https://github.com/basho/node_package/issues/47)
* node_package/49: [Create .pid files for package builds](https://github.com/basho/node_package/issues/49)
* node_package/50: [RPM %files changes behavior on Fedora 18](https://github.com/basho/node_package/issues/50)
* node_package/51: [Return nonzero exit codes on init function failure](https://github.com/basho/node_package/issues/51)
* node_package/54: [Fix %files section to not claim ownership of bindir and mandir](https://github.com/basho/node_package/issues/54)
* node_package/55: [Investigate shipping configuration to increase open files ulimit](https://github.com/basho/node_package/issues/55)
* node_package/56: [Name SunOS packages based on erlang architecture rather than uname](https://github.com/basho/node_package/issues/56)
* node_package/57: [Base architecture naming on erlc arch](https://github.com/basho/node_package/issues/57)
* node_package/60: [Add support for SmartOS 13.1](https://github.com/basho/node_package/issues/60)
* node_package/61: [add simple warnings on attach/attach-direct](https://github.com/basho/node_package/issues/61)
* node_package/63: [remove contract specification from SMF manifests (solaris)](https://github.com/basho/node_package/issues/63)
* node_package/65: [Create patches for SmartOS packages to handle differing behavior](https://github.com/basho/node_package/issues/65)
* erlang_protobuffs/41: [Fix some README example problems, callout deep lists change.](https://github.com/basho/erlang_protobuffs/issues/41)
* erlang_protobuffs/42: [Cleanup warnings](https://github.com/basho/erlang_protobuffs/issues/42)
* erlang_protobuffs/45: [Be more firewall-friendly :-)](https://github.com/basho/erlang_protobuffs/issues/45)
* erlang_protobuffs/46: [Fix parsing hex values](https://github.com/basho/erlang_protobuffs/issues/46)
* erlang_protobuffs/47: [fix compiler warnings about shadowed variables](https://github.com/basho/erlang_protobuffs/issues/47)
* erlang_protobuffs/49: [Fix enums when using packages](https://github.com/basho/erlang_protobuffs/issues/49)
* erlang_protobuffs/51: [Remove O(N^2) algorithm from repeated field extraction.](https://github.com/basho/erlang_protobuffs/issues/51)
* riak/254: [Changed `riak attach` to use a remsh](https://github.com/basho/riak/issues/254)
* riak/268: [Switch riak to use node_package for packaging](https://github.com/basho/riak/issues/268)
* riak/272: [Add new rebar binary and erlydtl opts info toplevel rebar.config for solving dialyzer glitches](https://github.com/basho/riak/issues/272)
* riak/283: [We insist on a minimum of 5 nodes in a cluster, adjust devrel](https://github.com/basho/riak/issues/283)
* riak/286: [move and clarify ulimit check](https://github.com/basho/riak/issues/286)
* riak/288: [remove embedded option ](https://github.com/basho/riak/issues/288)
* riak/290: [Support multiple PB listeners](https://github.com/basho/riak/issues/290)
* riak/294: [Add missing rm -rf dev/$@/lib/riaknostic on dev target](https://github.com/basho/riak/issues/294)
* riak/303: [update riak-admin transfers](https://github.com/basho/riak/issues/303)
* riak/310: [Remove incorrect `-embedded` flag from riak startup command](https://github.com/basho/riak/issues/310)
* riak/311: [add "cluster resize-ring <new-size>" to riak-admin](https://github.com/basho/riak/issues/311)
* riak/322: [Add riak-debug, a command for automating the collection of information for diagnosing problems.](https://github.com/basho/riak/issues/322)
* riak/329: [Riaknostic no longer escript. Alter build process accordingly.](https://github.com/basho/riak/issues/329)
* riak/331: [Lower net_ticktime to check for aliveness more often](https://github.com/basho/riak/issues/331)
* riak/339: [update vm.args for moving to OTP team scheduler patch](https://github.com/basho/riak/issues/339)
* riak/341: [update app.config to activate v1 object format on new installs](https://github.com/basho/riak/issues/341)
* riak/345: [Unable to build Riak from source tarball while offline](https://github.com/basho/riak/issues/345)
* riak_api/21: [Move setting/fetching bucket properties out of riak_kv](https://github.com/basho/riak_api/issues/21)
* riak_api/22: [Use init:script_id() for the server version.](https://github.com/basho/riak_api/issues/22)
* riak_api/23: [Enable multiple PB listeners.](https://github.com/basho/riak_api/issues/23)
* riak_api/24: [Add support for resetting bucket properties. Requires basho/riak_pb#35.](https://github.com/basho/riak_api/issues/24)
* riak_api/25: [Remove lager dependency because it is specified by riak_core.](https://github.com/basho/riak_api/issues/25)
* riak_api/28: [No PB listeners leads to repeated log messages concerning a failed stat calculation](https://github.com/basho/riak_api/issues/28)
* riak_control/54: [Fixes to get dialyzer working.](https://github.com/basho/riak_control/issues/54)
* riak_control/59: [Series of dialyzer and formatting changes.](https://github.com/basho/riak_control/issues/59)
* riak_control/71: [Add cluster management.](https://github.com/basho/riak_control/issues/71)
* riak_control/80: [Add ability to stop and down nodes.](https://github.com/basho/riak_control/issues/80)
* riak_control/81: [Normalize resource names.](https://github.com/basho/riak_control/issues/81)
* riak_control/83: [Make join node more explicit.](https://github.com/basho/riak_control/issues/83)
* riak_control/88: [Provide a default selection.](https://github.com/basho/riak_control/issues/88)
* riak_control/111: [Prevent badarith when memory is unavailable.](https://github.com/basho/riak_control/pull/111)
* riak_core/185: [inbound handoffs never cleanup](https://github.com/basho/riak_core/issues/185)
* riak_core/241: [potential fix for #185](https://github.com/basho/riak_core/issues/241)
* riak_core/270: [Dialyzer Fixes](https://github.com/basho/riak_core/issues/270)
* riak_core/274: [Allow parallel vnode initialization](https://github.com/basho/riak_core/issues/274)
* riak_core/282: [Extract out and export pending claim function.](https://github.com/basho/riak_core/issues/282)
* riak_core/284: [initial add of the Riak Core Connection Manager](https://github.com/basho/riak_core/issues/284)
* riak_core/290: [Add support for tracking progress of individual handoffs](https://github.com/basho/riak_core/issues/290)
* riak_core/291: [SSL support](https://github.com/basho/riak_core/issues/291)
* riak_core/297: [don't use hardcoded app names in SSL utils](https://github.com/basho/riak_core/issues/297)
* riak_core/298: [Race in vnode worker pool](https://github.com/basho/riak_core/issues/298)
* riak_core/299: [Vnode nonblocking reply, First draft (3rd edition), ready for some review](https://github.com/basho/riak_core/issues/299)
* riak_core/300: [Fix worker pool races](https://github.com/basho/riak_core/issues/300)
* riak_core/301: [Ring Resizing](https://github.com/basho/riak_core/issues/301)
* riak_core/302: [rework coverage fsm timeouts.](https://github.com/basho/riak_core/issues/302)
* riak_core/305: [Support for `plan/2` and `process_results/3` funs for coverage fsm](https://github.com/basho/riak_core/issues/305)
* riak_core/312: [Enhance transfer display + wrapping nodenames.](https://github.com/basho/riak_core/issues/312)
* riak_core/313: [format _stat_ts in connection manager](https://github.com/basho/riak_core/issues/313)
* riak_core/316: [handoff batching](https://github.com/basho/riak_core/issues/316)
* riak_core/319: [Optimize to better handle large rings/nodes](https://github.com/basho/riak_core/issues/319)
* riak_core/321: [proper return value for riak_core_console:transfers/1](https://github.com/basho/riak_core/issues/321)
* riak_core/322: [Handle node up/down in tcp_mon](https://github.com/basho/riak_core/issues/322)
* riak_core/323: [Permanently disable legacy gossip](https://github.com/basho/riak_core/issues/323)
* riak_core/325: [Fix a typo in tcp_mon init](https://github.com/basho/riak_core/issues/325)
* riak_core/328: [Fix overload test time outs](https://github.com/basho/riak_core/issues/328)
* riak_core/330: [dont start coverage timeout timer if timeout is infinite](https://github.com/basho/riak_core/issues/330)
* riak_core/331: [fix forced_ownership_handoff during resize](https://github.com/basho/riak_core/issues/331)
* riak_core/332: [update bad value protection for timer value](https://github.com/basho/riak_core/issues/332)
* riak_core/334: [Reporting 'normal' events is spammy, don't do it](https://github.com/basho/riak_core/issues/334)
* riak_core/336: [Fix crashing stat mod never getting rescheduled](https://github.com/basho/riak_core/issues/336)
* riak_core/339: [Fix repair handoff crash, missing not sent fun](https://github.com/basho/riak_core/issues/339)
* riak_core/340: [only silently drop DOWN-normal messages in deleted modstate](https://github.com/basho/riak_core/issues/340)
* riak_kv/30: [Bz982 - js_reload not working](https://github.com/basho/riak_kv/issues/30)
* riak_kv/31: [Key count reduce function](https://github.com/basho/riak_kv/issues/31)
* riak_kv/334: [Every read triggers a read-repair when Last-write-wins=true](https://github.com/basho/riak_kv/issues/334)
* riak_kv/385: [Objects cannot be updated if a bad CRC is encountered by Bitcask](https://github.com/basho/riak_kv/issues/385)
* riak_kv/462: [Expose FSM timeouts via the HTTP API](https://github.com/basho/riak_kv/issues/462)
* riak_kv/467: [add stats for coverage query starts](https://github.com/basho/riak_kv/issues/467)
* riak_kv/479: [More Compact Riak Object Binary Format](https://github.com/basho/riak_kv/issues/479)
* riak_kv/487: [provide a Location header for the same api version on POST](https://github.com/basho/riak_kv/issues/487)
* riak_kv/488: [Move setting/fetching bucket properties to riak_api](https://github.com/basho/riak_kv/issues/488)
* riak_kv/489: [Migrate mapred_test to riak_test](https://github.com/basho/riak_kv/issues/489)
* riak_kv/491: [Remove Link headers from bucket and key lists](https://github.com/basho/riak_kv/issues/491)
* riak_kv/492: [Make hashtree_eqc close trees before destroy.](https://github.com/basho/riak_kv/issues/492)
* riak_kv/495: [Add encoding capability for handoff.](https://github.com/basho/riak_kv/issues/495)
* riak_kv/496: [Standardize KV backend responses and handling](https://github.com/basho/riak_kv/issues/496)
* riak_kv/498: [Document the environment in the logs; advise on bad settings.](https://github.com/basho/riak_kv/issues/498)
* riak_kv/500: [vclock capability](https://github.com/basho/riak_kv/issues/500)
* riak_kv/510: [Remove merge_index dependency and unused erl_first_file.](https://github.com/basho/riak_kv/issues/510)
* riak_kv/512: [Protocol Buffers interface allows the creation of records with an empty key](https://github.com/basho/riak_kv/issues/512)
* riak_kv/520: [Adds X-Riak-Deleted where missing](https://github.com/basho/riak_kv/issues/520)
* riak_kv/521: [Changes needed to expose FSM timeouts to clients](https://github.com/basho/riak_kv/issues/521)
* riak_kv/526: [Expose Backend Size to Handoff for Progress Tracking](https://github.com/basho/riak_kv/issues/526)
* riak_kv/527: [List buckets timeout & streaming](https://github.com/basho/riak_kv/issues/527)
* riak_kv/529: [Count async MR results against the sink buffer size cap](https://github.com/basho/riak_kv/issues/529)
* riak_kv/530: [Ring Resizing Support](https://github.com/basho/riak_kv/issues/530)
* riak_kv/532: [kv_wm_utils expects ?MD_DELETED to be "true" not 'true'](https://github.com/basho/riak_kv/issues/532)
* riak_kv/536: [A simple way to store a PN-Counter in a riak_object](https://github.com/basho/riak_kv/issues/536)
* riak_kv/542: [add license header to riak_kv reformat](https://github.com/basho/riak_kv/issues/542)
* riak_kv/546: [Expose the put_fsm 'asis' option to clients](https://github.com/basho/riak_kv/issues/546)
* riak_kv/552: [Add new backend capability for Riak r_object use](https://github.com/basho/riak_kv/issues/552)
* riak_kv/554: [Optimize to better handle large rings/nodes](https://github.com/basho/riak_kv/issues/554)
* riak_kv/555: [Add init/final to AAE remote interface](https://github.com/basho/riak_kv/issues/555)
* riak_kv/559: [Alter env recommendations for 1.4](https://github.com/basho/riak_kv/issues/559)
* riak_kv/560: [Fix regression in 2i reformat status flag & add extra status function](https://github.com/basho/riak_kv/issues/560)
* riak_kv/562: [use old object format by default on upgrade](https://github.com/basho/riak_kv/issues/562)
* riak_kv/563: [Add binary format for counters](https://github.com/basho/riak_kv/issues/563)
* riak_kv/569: [Make sure client supplied N <= bucket N](https://github.com/basho/riak_kv/issues/569)
* riak_kv/576: [Add a capability for counters](https://github.com/basho/riak_kv/issues/576)
* riak_kv/579: [Skip start {val, key} pair if start_inclusive is false](https://github.com/basho/riak_kv/issues/579)
* riak_kv/581: [Wire up sidejob stats to /stats endpoint](https://github.com/basho/riak_kv/issues/581)
* riak_kv/585: [minor improvements to riak_object downgrade support](https://github.com/basho/riak_kv/issues/585)
* riak_kv/586: [Stop fold when a vnode reaches page size](https://github.com/basho/riak_kv/issues/586)
* riak_kv/587: [Fix incorrect arg in call to get_primary_apl/3 by put FSM](https://github.com/basho/riak_kv/issues/587)
* riak_kv/588: [Multi backend was missing data_size function](https://github.com/basho/riak_kv/issues/588)
* riak_pb/30: [Add remaining bucket properties to PBC](https://github.com/basho/riak_pb/issues/30)
* riak_pb/31: [Fix errors with repl bucket property.](https://github.com/basho/riak_pb/issues/31)
* riak_pb/32: [precommit/postcommit empty does not clear](https://github.com/basho/riak_pb/issues/32)
* riak_pb/33: [Fix commit hooks and symbolic properties as binaries](https://github.com/basho/riak_pb/issues/33)
* riak_pb/35: [Support reset bucket properties feature.](https://github.com/basho/riak_pb/issues/35)
* riak_pb/36: [Protoc dependency free version for Python Package Index](https://github.com/basho/riak_pb/issues/36)
* riak_pb/38: [Add timeouts to get, put, and delete](https://github.com/basho/riak_pb/issues/38)
* riak_pb/41: [Add messages for exporter tool & list timeouts](https://github.com/basho/riak_pb/issues/41)
* riak_pb/42: [Remove need to have protoc available in Python source package. Closes #36](https://github.com/basho/riak_pb/issues/42)
* riak_pb/43: [2i pagination support](https://github.com/basho/riak_pb/issues/43)
* riak_pb/44: [Add asis flag for RpbPutReq.](https://github.com/basho/riak_pb/issues/44)
* riak_pipe/50: [fitting was done before startup](https://github.com/basho/riak_pipe/issues/50)
* riak_pipe/62: [Move eunit system tests to riak_test](https://github.com/basho/riak_pipe/issues/62)
* riak_pipe/68: [assume handoff if vnode exits 'normal' during queue requeuest](https://github.com/basho/riak_pipe/issues/68)
* riak_pipe/71: [Fix opaque type warnings on R16B.](https://github.com/basho/riak_pipe/issues/71)
* riak_pipe/73: [PULSE test & fix riak_pipe_fitting](https://github.com/basho/riak_pipe/issues/73)
* riak_pipe/75: [lower "fitting was gone" log to debug level](https://github.com/basho/riak_pipe/issues/75)
* riak_pipe/76: [limited support for ring resizing](https://github.com/basho/riak_pipe/issues/76)
* riak_search/140: [remove guard on riak_search_vnode:start_vnode/1](https://github.com/basho/riak_search/issues/140)
* riaknostic/55: [Remove misplaced parathesis (sysctl check)](https://github.com/basho/riaknostic/issues/55)
* riaknostic/56: [Add OpenBSD bits](https://github.com/basho/riaknostic/issues/56)
* riaknostic/66: [Un-escriptize riaknostic and modify for lager 2.0 compatability](https://github.com/basho/riaknostic/issues/66)
* riaknostic/67: [Add an extra log line for clarity when running non-existent checks](https://github.com/basho/riaknostic/issues/67)
* webmachine/76: [Add logging for when webmachine crashes and body exists.](https://github.com/basho/webmachine/issues/76)
* webmachine/115: [Fix arguments to call to webmachine_request:recv_stream_body/2](https://github.com/basho/webmachine/issues/115)
* webmachine/117: [Decode Content-MD5 with base64, not hex](https://github.com/basho/webmachine/issues/117)
* webmachine/124: [Refine range header treatment](https://github.com/basho/webmachine/issues/124)
* webmachine/125: [Guess text/css MIME type for .less files](https://github.com/basho/webmachine/issues/125)
* webmachine/128: [Bugfix for multiple routers under Riak](https://github.com/basho/webmachine/issues/128)
* webmachine/134: [collapse 4 separate send calls into 1 in send_chunk](https://github.com/basho/webmachine/issues/134)
* webmachine/141: [Custom reason phrase](https://github.com/basho/webmachine/issues/141)
* webmachine/142: [Read body when DELETE to keep alive connection](https://github.com/basho/webmachine/issues/142)
* webmachine/143: [webmachine_dispatcher crashes on malformed Host header](https://github.com/basho/webmachine/issues/143)
* webmachine/144: [Allow responses for all HTTP errors to be customized](https://github.com/basho/webmachine/issues/144)
* webmachine/151: [Removing io:format/2 calls from log file processing](https://github.com/basho/webmachine/issues/151)


## 既知の問題 (Known Issues)

### leveldb 1.3 から 1.4への変換 (leveldb 1.3 to 1.4 conversion)

leveldb 1.3.x、1.2.xのデータを使ってleveldb 1.4.0を初めて起動すると、データの自動変換が始まります。これは、各ノードの起動を3分から7分停止する可能性があります。”level #1”内のleveldbのデータは”level #1”がソートされたデータレベルの代わりに、重複したデータレベルとしての役割を果たすよう、調整されてゆきます。この変換は”level #1”から”level #2”への通常のコンパクションを通じて、単に”level #1”内のファイル数を８より小さくするものです。これは一度だけ実行されます。

## 廃止予定 (Deprecation Warnings)

### Ubuntu 11.04 (Natty) EOL

Ubuntu 11.04 Natty Narwhal は2012年10月でend-of-lifeになり、apt update、securityの公開リポジトリは削除されています。
これによりRiakは今後11.04に対してビルドされません。私たちは次のRiakのメジャーバージョンリリースのタイミングで、
最新のnon-LTSリリースに対するサポートを検討する予定です。

UbuntuのLTSリリース(10.04、12.04)は引き続きサポートされますので影響はありません。
