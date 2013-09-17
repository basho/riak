## Riak 1.3.2 リリースノート

### Riakの新機能と主な改善点

#### eLevelDB のコンパクションの整合性チェック

Riak 1.2で、我々はleveldbの壊れたブロックを lost/BLOCKS.bad に、コンパクション中に隔離するようにしました。これは、コンパクションのために A)リードリペアとAAEが裏側でデータをデータを修正しうるにもかかわらず、 B)手動でこれを修正する顧客サポートのエンジニアの工数がかかることを防ぐためです。

残念なことに、ふたつあるデータ破壊チェックのうちひとつしかコンパクション中に動いていなかったことに気づいていませんでした。ファイルのメタデータも含めて全てのブロックに対するCRCチェックです。圧縮のロジックでは、圧縮されたデータブロックに対して hash によるチェックが入っています。CRCチェックの方はデフォルトでは有効になっていません。悲しいことに、 leveldb はCRC以上のことは非常に限られたことしかしません。もし壊れたブロックがたまたま圧縮のhashチェックで全て検出できればから運がよいものの、壊れたディスクのファイルを読むと leveldb / Riak のクラッシュもありえました。

これに対するGoogleの対策はデフォルトでオフになっている `paranoid_checks` オプションです。残念ながらこれを `true` にするとコンパクション時のCRCチェックが有効になるだけでなく、リカバリログのCRCチェックも有効になってしまいます。プロセスが落ちた後のリカバリログのCRCチェックは失敗することがありえます。これは既存のコードでも、次に起動したときに活用されます。 `paranoid_checks` オプションが `true` になっていると自動リカバリ（の際の修復）が動きません。これは望ましくない挙動です。

そこで、新しいオプション `verify_compactions` を設けました。`paranoid_checks` で切り替えていた、バックグラウドで動作するCRCチェックはこの新しいオプションで設定できます。リカバリログのCRCチェックはこれからも `paranoid_checks` で設定されます。 `verify_compactions` のデフォルトは `true` です。 `paranoid_checks` のデフォルトは `false` のままです。

**注意:** CRC計算は高コストです。Riak 1.3 でIntelのハードウェアCRC回路が有効な場合はそれを使うコードが追加されました。Riak 1.2では複数の優先度付けされたスレッドが動作するようになっています。これらの2つの機能によって、バックグラウドのコンパクション処理中の高価なCRC計算のコストの影響を最小限に留めることができています。

#### Erlang スケジューラ縮退

R16B01以前の全てのErlang/OTPのリリースでは、Erlangのスケジューラがスリープしやすすぎる
という問題があります。スリープすることによって、その間の電力消費やスレッド間の
リソース競合を減らすことができます。

このリリースのRiak EDSはErlang/OTPの仮想マシンに、スリープしている
スケジューラを通常の間隔で起こすためのパッチを必要とします。 `+sfwi` というフラグが
`vm.args` ファイルに入っている必要があります。この値はミリ秒です。アプリケーションに
よってはチューニングが必要になると思います。オープンソース版のRiakでも、
このパッチ（ `vm.args` の追加フラグ）は推奨されています。パッチは
https://gist.github.com/evanmcc/a599f4c6374338ed672e にあります。

#### 過負荷対策 / 負荷制御

Riak 1.3.2 で、過負荷を防御する仕組みを組み込みました。もしRiakノードが
過負荷状態になると、 `{error, overload}` というレスポンスを返すことで
リクエストがキューに積まれたまま放置することはなくなりました。

これまでは、Riakは常にリクエストをキューに保持してきました。過負荷状態が
悪化すると、キューに積まれたリクエストはタイムアウトするまでキューに積まれた
ままでした。最終的にはRiakのノードは応答できなくなり、クラッシュしていました。

新しい過負荷対策はこのために用意されました。

`app.config` で設定可能です。デフォルトの設定は様々なクラスタのサイズでテストされ、
処理可能なリクエスト数はRiakの全てのユーザーを満足させるものです。しかし、
完璧を期して以下に説明しておきます。

Riakには2種類の過負荷対策が組み込まれています。どちらも異なる設定です。
ひとつめはノードあたりの get と put の同時実行数を制限するものです。これは
`riak_kv/fsm_limit` という項目で設定できます。デフォルトは `50000` です。
この最大値は get と put それぞれで制御されますので、デフォルトではトータルで
`100000` まで扱えることになります。

ふたつめの過負荷対策は、各 vnode 毎のメッセージキューの長さを制限するものです。
これは `riak_core/vnode_overload_threashold` という項目で設定できます。
デフォルトは `10000` です。

どちらの設定も、 `app.config` で `undefined` とすると過負荷対策それ自体を
無効化することができますが、これは推奨しません。 `app.config` から項目を消すなど
して設定しなかった場合、前述のデフォルト値が採用されます。

過負荷対策と同時に `/stats` に表示される新しい統計値を導入しました。

`dropped_vnode_requests_total` の統計値は vnode の過負荷制御部分で無視された
メッセージの数を表しています。

get/put の過負荷対策については、いくつか新しい統計値があります。以下のものは
get についてのものですが、同様のものが put についても追加されています。

`node_get_fsm_active` と `node_get_fsm_active_60s` の統計値は
そのノード上のそれぞれ直近1秒、1分で有効な get のリクエスト数を表します。
`node_get_fsm_in_rate` と `node_get_fsm_out_rate` は直近1秒で
リクエスト処理の開始と終了を終えたものの数をあらわします。さいごに、
`node_get_fsm_rejected`, `node_get_fsm_rejected_60s` と
`node_get_fsm_rejected_total` はそれぞれの時間幅で無視されたリクエスト数を表します。

#### ヘルスチェック無効化

ヘルスチェックの機能は Riak 1.3.0 でリリースされましたが、 1.3.2 で無効化されました。
過負荷対策の仕組みが同様の役割をより安全に果たしているからです。特に、ヘルスチェックの
アプローチは、ノードが遅いために発生してしまった過負荷状態からはうまく復帰できましたが、
クラスタ全体の処理能力を超えた負荷スパイクに対しては脆弱でした。つまり、2番目の場合には、
ヘルスチェックのアプローチ（負荷を別のノードに分担する）は、問題を悪化させます。

### 解決された Issues / PR

* riak/306: [Wrong ERTS_PATH configuration on ubuntu riak package](https://github.com/basho/riak/issues/306)
* riak/327: [Increase ERL_MAX_PORTS and ERL_MAX_ETS_TABLES](https://github.com/basho/riak/pull/327)
* riak/333: [Add health check deprecation note to config](https://github.com/basho/riak/pull/333)
* riak/336: [Update default configs for 1.3.2](https://github.com/basho/riak/pull/336)
* bitcask/83: [Improve fold speed for large files filled with small objects.](https://github.com/basho/bitcask/issues/83)
* bitcask/84: [eunit test enhancements.](https://github.com/basho/bitcask/issues/84)
* bitcask/87: [Change behavior of merge when merging for data expiration](https://github.com/basho/bitcask/issues/87)
* eleveldb/55: [Export types to avoid opaque type warning in R16.](https://github.com/basho/eleveldb/issues/55)
* eleveldb/59: [Issue 58:  Address condition where multiple erlang threads could take ownership of ...](https://github.com/basho/eleveldb/issues/59)
* eleveldb/83: [create new option specifically for compaction CRC test control](https://github.com/basho/leveldb/pull/83)
* leveldb/72: [allow Get() calls to avoid copies into std::string](https://github.com/basho/leveldb/issues/72)
* leveldb/83: [create new option specifically for compaction CRC test control](https://github.com/basho/leveldb/pull/83)
* riak_core/298: [Race in vnode worker pool](https://github.com/basho/riak_core/issues/298)
* riak_core/299: [Vnode nonblocking reply, First draft (3rd edition), ready for some review](https://github.com/basho/riak_core/issues/299)
* riak_core/300: [Fix worker pool races](https://github.com/basho/riak_core/issues/300)
* riak_core/307: [Backport issue 300 to 1.3 branch](https://github.com/basho/riak_core/issues/307)
* riak_core/314: [Implement vnode overload protection](https://github.com/basho/riak_core/issues/314)
* riak_core/317: [Backport of #299 to 1.3](https://github.com/basho/riak_core/issues/317)
* riak_core/338: [Fix crashing stat mod never getting rescheduled](https://github.com/basho/riak_core/pull/338)
* riak_pipe/73: [PULSE test & fix riak_pipe_fitting](https://github.com/basho/riak_pipe/issues/73)
* riak_pipe/74: [Backport fix in issue 73 to 1.3 branch](https://github.com/basho/riak_pipe/issues/74)
* riak_kv/544: [Bound the number of get/put FSMs to prevent overload](https://github.com/basho/riak_kv/issues/544)
* riak_kv/547: [Changes to support vnode overload protection](https://github.com/basho/riak_kv/issues/547)
* riak_kv/549: [Deprecate KV health checks](https://github.com/basho/riak_kv/issues/549)
* riak_kv/556: [Enable stat updates to use sidejob workers](https://github.com/basho/riak_kv/issues/556)
* riak_kv/558: [Add fsm active and error stats to the blob](https://github.com/basho/riak_kv/issues/558)
* riak_kv/565: [Fix put_fsm_eqc after local_put_failed change](https://github.com/basho/riak_kv/pull/565)
* riak_kv/581: [Wire up sidejob stats to /stats endpoint](https://github.com/basho/riak_kv/pull/581)

### 既知の問題
* riak_kv/400 If the node owns fewer than 2 partitions, the following warning will appear in the logs
  `riak_core_stat_q:log_error:123 Failed to calculate stat {riak_kv,vnode,backend,leveldb,read_block_error} with error:badarg`
      [Fixed in master](https://github.com/basho/riak_kv/issues/470)

## Riak 1.3.1 リリースノート

### Riakの新機能と主な改善点

#### 2i Big Integerエンコーディング

1.3.1以前の全てのバージョンのRiakでは、2iのintの範囲指定は 2147483647 (0x7fffffff)
以上の結果を全て返すことができませんでした。これはRiakが内部でデータをeleveldbに
格納するためのエンコーディングライブラリ sext [1] の問題に由来するものと判明しました。
sext はソート順を保ったままErlangのタームをバイナリにシリアライズするものです。
大きな整数の場合はこれがうまくいきませんでした。
2iの実装がこの機能に依存しているため、範囲検索が影響を受けていました。

sextの問題は修正され [2] 、Riak 1.3.1 に含まれています。新しくRiak 1.3.1 を
インストールした場合はこの恩恵を受けることができますが、大きな整数のエンコーディングは
多少の非互換性をもたらします。1.3以前のRiakを使ってディスクにすでに書かれた
2147483647 以上の整数のインデックスがある場合には、正しい値を返すために上書きする必要があります。

Riak 1.3.1は `riak-admin` の一部としてノードの稼働中にインデックスを上書きする
ツールを含んでいます。インデックスが全てのノードで上書きされたあとは、範囲検索は
正しい結果を返すようになるでしょう。このツールは、大きな整数をインデックスを含んでいる
かどうかにかかわらず、2i を使うクラスタが全て 1.3.1 にアップグレードしたあとに
適用されなければなりません。

Riak ノードでこの上書きを実行するためには:

```
riak-admin reformat-indexes [<concurrency>] [<batch size>]
```

concurrencyオプションは同時に何個のパーティションを上書きするかを決めます。
特に指定されない場合はデフォルトで2となります。 batch size は一度に何個の
キーを変更するかを指定します。デフォルトは100です。 *負荷がかかっていなければ*
concurrencyを上げると、もっと早く終わらせることができるでしょう。 batchを
下げると、負荷がかかっている場合に他の操作のレイテンシが改善するでしょう。
上書きが終了すれば、結果はログに表示されます。 *もし上書きが失敗していたら再実行してください。*
この場合、上書きしていないキーだけを上書きしようとします。

もし1.3.1から1.3へのダウングレードをしたい場合には、ダウングレード後も正しく動作するために
インデックスを古いエンコーディングに再フォーマットしなおさないといけません。
これをするためには、`--downgrade` フラグを `riak-admin reformat-indexes` に与えます。

```
riak-admin reformat-indexes [<concurrency>] [<batch size>] --downgrade
```

concurrencyとbatch sizeのパラメータはアップグレードの場合と同様です。

[1] https://github.com/uwiger/sext

[2] https://github.com/uwiger/sext/commit/ff10beb7a791f04ad439d2c1c566251901dd6bdc

#### bitcaskの起動時間の改善

vnodeを並列に起動しない問題を改善しました。マルチコアのマシンで動作するとき、
バックエンドにbitcaskを使っている場合は起動時間がかなり改善するでしょう。
我々のクラスタでは桁違い(~10倍)の性能が出たこともあります。

#### PR/PW の挙動

これまでのRiakでは get と put の PR/PW が指定された場合、指定された数のプライマリが
オンラインかどうかだけをチェックしていましたが、 vnode が本当に応答したかどうかまでは確かめて
いませんでした。もしPW=2だった場合、ひとつのプライマリとひとつのフォールバックに書き込み成功すれば、
もうひとつのプライマリへの書き込みに失敗しても成功の応答を返していました。

Riak 1.3.1では、PRとPWでは、指定された数のプライマリが結果を返すまでは待つようになりました。
これは、 PR+PW > N かつ全てのリクエストが成功したら書き込んだデータが必ず読めるようになったことを
保証するということです（その間に他の書き込みや、修復不能なレプリカ障害がない限り）。

失敗したPWは用意に部分書き込みになりうることを忘れないでください。この変更は純粋に、
read/write の成功で保証される内容を強化したものです。詳細は以下のプルリクエストのリンクをご覧ください。


### 解決された Issues / PR

* riak_kv/505: [Fix bug where stats endpoints were calculating _all_ riak_kv stats](https://github.com/basho/riak_kv/issues/505)
  NOTE: this fix introduces a slight change to the stats caching strategy in riak. Formerly stats were cached for TTL seconds
  and the cache's freshness checked when a request to a stats endpoint was serviced. If the cache was stale the stats would be
  calculated on demand. From 1.3.1 forward all stats requests are served from the cache. A background process calculates stats
  and refreshes the cache at an interval. This smooths the access latency for stats. A new stat  `{riak_kv_stat_ts, timestamp()}`
  is added to the returned stats that indicates the time the stats were calculated.
* riak_kv/508: [If a `folsom_metrics_histogram_ets` owned table dies, kv_stat cannot recreate it](https://github.com/basho/riak_kv/issues/508)
  NOTE: introduces the stat value `unavailable` for any stat that cannot be calculated due to an error. Previously a call to a stats endpoint
  would simply fail, with this fix, failed stats are `unavailable` and all others returned uneffected.
* riak_api/26: [Fix stat names so delete of stats on restart works](https://github.com/basho/riak_api/issues/26)
* riak_core/281: [Porting parallel vnode init fix to 1.3 + revert switch](https://github.com/basho/riak_core/issues/281)
* riak_core/288: [Failure to calculate a stats value should be temporary so warn only](https://github.com/basho/riak_core/issues/288)
* riak_pipe/70: [Fix stat names so delete of stats on restart works](https://github.com/basho/riak_pipe/issues/70)
* riak_kv/485: [Fix PR/PW](https://github.com/basho/riak_kv/issues/485)
* riak_kv/499: [Big integer 2i indexes sometimes sort incorrectly](https://github.com/basho/riak_kv/issues/499)
* riak_kv/511: [riak_kv 1.3.1 using parallel vnode init](https://github.com/basho/riak_kv/issues/511)
* riak_kv/514: [Change AAE to use incremental crypto:sha calculations](https://github.com/basho/riak_kv/issues/514)
* riak_kv/516: [support for handling legacy sext encoding of 2i keys](https://github.com/basho/riak_kv/issues/516)
* riak_kv/517: [Since stats now get repaired when an update fails, log as `warning`](https://github.com/basho/riak_kv/issues/517)
* riak_kv/522: [Spell badarg correctly](https://github.com/basho/riak_kv/pull/522)
* riak_kv/523: [Fix perf problems and bug in 2i reformat](https://github.com/basho/riak_kv/pull/523)
* riak_kv/525: [move querying of fixed index status to seperate backend function](https://github.com/basho/riak_kv/pull/525)
* riak/302: [Add batch size param to 2i reformat cmd](https://github.com/basho/riak/pull/302)
* bitcask/86: [Fix race with deleting stale input files from merge](https://github.com/basho/bitcask/pull/86)


## Riak 1.3.0 リリースノート

### Riakの新機能と主な改善点

#### アクティブ・ アンチエントロピー(Active Anti-Entropy)

Riak 1.3の新機能です。 Riakは今回アクティブ・アンチエントロピー(AAE) サブシステムを組み込みました。これはRiakクラスター全体に渡るデータの検証と修復を行うものです。
AAEシステムはデータの欠落、不一致を確認するために、データレプリカ間で定期的に情報を交換します。不良レプリカが見つかると、これを直すためにAAEはread repairを実行します。AAEは完全に自動化されており、多くのデータ消失シナリオ（ディスク故障、古いバックアップからのリストア、bit不良など）を防ぐ新たなレイヤーとなります。

AAEはハッシュツリー交換を使って実装されています。これによりデータレプリカ間で交換されるデータ量は、Riakに保存された全データではなく、不整合となっているデータに比例します。すべてのデータが同期されている場合(通常の状態)、素早く、かつ極めて低いオーバーヘッドで情報を交換します。このためAAEは、クラスタへほぼ影響を与えることなく、１分間に複数回の交換を行えます。

AAEのハッシュツリーは通常のRiak K/Vデータとは分離されたLevelDBインスタンスに永続化されます。まっさらなRiak1.3クラスタを初めて起動すると（もしくは以前のリリースからアップグレードすると）各パーティションデータを参照することでハッシュツリー情報を生成します。デフォルトでRiakは各ノードで１時間毎にひとつのハッシュツリーを生成します。パーティションデータの参照に一時間以上かかる場合、Riakは必要に応じて次のハッシュツリー生成を開始します。ただしデフォルトでは同時に生成するハッシュツリーは2つまでです。

一度ハッシュツリーが作られると、Riakへ送られるwriteに従って最新の状態に保たれます。しかしK/Vデータとハッシュツリー間の乖離を防ぐために、ツリーは定期的に期限が切れて無効となり、再生成されます。ツリーの再生成はbit不良のような見えにくいデータ破損も防ぎます。デフォルトでツリーは１週間で期限が切れて再生成されます。

今まで述べた内容（およびその他の項目）は `app.config` にて設定できます。AAEに関する設定項目は `riak_kv` セクションにあり、どのようなオプションが選択可能かについてのコメントがついています。

AAEの動作状況を知るのに、Riakは `riak-admin aae-status` コマンドを提供します。AAEのステータス出力は、エクスチェンジ、エントロピーツリー、そして修復した鍵の3項目に分かれます。

```
================================== Exchanges ==================================
Index                                              Last (ago)    All (ago)
-------------------------------------------------------------------------------
0                                                  3.8 min       4.1 min
91343852333181432387730302044767688728495783936    3.3 min       7.8 min
182687704666362864775460604089535377456991567872   2.8 min       8.3 min
274031556999544297163190906134303066185487351808   2.3 min       6.3 min
365375409332725729550921208179070754913983135744   1.8 min       5.5 min
<snip>
```
エクスチェンジの項目では、各K/Vパーティション間のAAEによる情報の交換についての情報を示しています。 `Last` の列は、パーティションとそのsiblingレプリカのどれかの間で、いつ最も新しい情報の交換が行われたかについて示しています。 `All` の列では、パーティションがすべてのsiblingレプリカとやり取りを終えてからどれだけ経っているかについて示しています。簡単にいえば、`All` の列はそれぞれのパーティションがどれだけ古くなっているかについての時間の上限を示しています。具体的には、パーティションの中のデータについて、そのデータのすべてのレプリカが無効になっていない限り、 `All` で示された値よりも古いデータでは失われたり不整合が起こっていたりすることはないということです。

```
================================ Entropy Trees ================================
Index                                              Built (ago)
-------------------------------------------------------------------------------
0                                                  22.1 min
91343852333181432387730302044767688728495783936    22.6 min
182687704666362864775460604089535377456991567872   22.3 min
274031556999544297163190906134303066185487351808   22.9 min
365375409332725729550921208179070754913983135744   22.3 min
<snip>
```

エントロピーツリーの項目では、パーティション毎に、いつハッシュツリーが作られたかを示しています。AAEの情報の交換に参加する前に、各パーティションにはハッシュツリーが作られなければなりません。前述の通り、これらのツリーは作成後（デフォルトでは）1週間で無効となり再作成されます。

```
================================ Keys Repaired ================================
Index                                                Last      Mean      Max
-------------------------------------------------------------------------------
0                                                     0         0         0
91343852333181432387730302044767688728495783936       87        21        87
182687704666362864775460604089535377456991567872      0         0         0
274031556999544297163190906134303066185487351808      0         0         0
365375409332725729550921208179070754913983135744      0         0         0
<snip>
```

修復した鍵の項目では、AEによって成された修復についての情報を示しています。これには最新の情報の交換で修復された鍵の数、またすべての情報の交換でなされた修復についての平均値と最大値が含まれています。

注: すべてのAAEの状態情報はメモリ中にあり、ノードの再起動でリセットされます。ツリーの作成情報のみが永続的に残ります。これはツリー自身が永続的だからです。

AAEに関する注意事項:

1. ツリーは情報の交換が起こる前に作成されていなければなりません。ツリーはデフォルトでは1時間に1度作られるため、すべてのツリーが作られるまでには1.3について最初の起動またはアップグレードが起こってから 「`ring_size / number_of_nodes` 時間」かかります。そしてこの時間がAAEがすべてのデータを保護するまでにかかる時間となります。

2. 一般にツリーの作成にはCPU一つを可能ならば100%使用しますが、Riakの性能には最小限の影響で済みます。BitcaskをK/Vデータに使う際、ツリーの作成中は `list_keys`, `list_buckets`, および Riak EEの fullsync 複製戦略にかかる遅延時間が増える可能性があります。一度ツリーができれば、1週間後にツリーが無効になり再作成されるまで、これらの問題は起こることはありません。

3. データの不整合や損失のない正常なクラスタでも、AAEでは時々少量（1つか2つ）の鍵を修復することがあります。これはあるノードに対しての書き込みが発生している際に、AAEがその同じノードに同時に情報の交換をする際に起こります。例えば、ある書き込みがノードAに到達済みでノードBへ向かっている途中にAAEが実行されると、ノードAへの書き込みは見えてもノードBに対するものは見えないため、強制的に修復を行います。AAEは読み込み修復のための読み込みしか要求しませんから、この振る舞いは全く安全です。

4. AAEはRiak K/Vの機能であり、Riak Searchのデータは保護しません。

#### MapReduce Sink バックプレッシャー

* [RiakKV - MapReduce Sink Backpressure](https://github.com/basho/riak_kv/pull/429), supported by [RiakPipe - Sink Type FSM](https://github.com/basho/riak_pipe/pull/59)

MapReduce には Riak Pipe によりステージ間バックプレッシャーが導入されました。Riak 1.3 より前は sink まではバックプレッシャーは適用されませんでした。PB/HTTP エンドポイントは pipe の出力レートをすべて捌けると仮定されていたためです。Riak 1.3 では、 sink までバックプレッシャーを拡張し、エンドポイントでも処理あふれがなくなりました。バックプレッシャーは sink バッファのソフト上限と、ワーカによる上限チェック間隔にてチューニング可能です。これらは Riak コンソールから application 環境変数を設定するか、app.config ファイルの riak_kv セクションで設定できます (以下、デフォルトを示します):

    {riak_kv,
     ...
     %% Soft cap on the MapReduce sink's buffer,
     %% expressed as a positive integer number of messages
     %% (one message is used per MapReduce result)
     %% MapReduce sink バッファのソフト上限。
     %% メッセージ数を正整数で設定します。
     %% (MapReduce 結果ひとつがメッセージひとつに対応します)
     {mrc_sink_buffer, 1000},

     %% Period at which a MapReduce worker must check
     %% the sink's buffer cap, expressed as an integer
     %% number of messages to send before waiting on
     %% an clear-to-send acknowledgement
     %%   0 = wait for acknowledgement of each message
     %%   1 = wait every other message
     %%   'infinity' = never wait for acknowledgements
     %% MapReduce ワーカが sink のバッファ上限をチェックする間隔。
     %% 送信可能確認応答を待つまでのメッセージ数を整数で指定します。
     %%   0 = すべてのメッセージに対して確認応答をまちます
     %%   1 = メッセージひとつおきに確認応答を待ちます
     %%   ‘infinity’ = 確認応答を待ちません

     {mrc_sink_sync_period, 10}
    }.

#### IPv6 サポート追加

* [Allow gen_nb_server to support IPv6 addresses - riak_core #249](https://github.com/basho/riak_core/pull/249)

Riak Handoff と Protocol Buffers インターフェイスは IPv6 アドレスで待受可能になりました(HTTP インターフェイスはすでに IPv6 をサポートしています)。アドレスの指定は `"::1"` (localhost を表します) のような文字列形式でも、`{0,0,0,0,0,0,0,1}` (同じく localhost を表します) のような 8-タプルによる 16 バイトアドレスの指定でも可能です。IPv4 アドレスもどちらの形式でも表せます(ただし後者の形式は 4-タプルで 4 バイトです)。 注意: Riak ノード名には適用されません。クラスタメンバー管理の IPv6 サポートについては、 `inet_dist_*` 設定を参照ください。 [Erlang documentation](http://www.erlang.org/documentation/doc-5.9.1/lib/kernel-2.15.1/doc/html/kernel_app.html)

#### Luke 削除

* [Remove Luke Usage](https://github.com/basho/riak_kv/pull/433)

Riak 1.2 リリースで廃止予定とされた luke application は、このリリースで取り除かれました。

#### `riak getpid` 追加

`riak stop` にバグ(修正されたバグ一覧に入っています)があり、修正の際に Riak 自身の PID 取得方法を変更しました。バグを修正しているとき、Riakで提供されていないスクリプトに頼りたくないシステム管理者にとっては、getpidはとても便利に思えたためです。 `riak getpid` ではその名の通り実行中の Riak の PID を返すようにしました。失敗時には終了コード 1 で終了します。これは小さな機能ですが `ps` や、 `grep`, `awk` を使う時間を節約してくれるでしょう。  

#### Riaknositic のデフォルト同梱

Riaknostic が Riak パッケージに含まれるようになり、利用しやすくなりました。1.3 より前のバージョンでは、ユーザは riaknostic を別途ダウンロードする必要がありましたが、1.3 では `riak-admin diag` がすぐに使えます。

#### SmartOS 1.8 の追加サポート

SmartOS 1.6 に加え、1.8 向けのパッケージが利用できるようになりました。

#### ヘルスチェック

Riak 1.3 で新規導入されました。Riak Core はヘルスチェックサブシステムを含むようになり、ノードを特定の条件で監視し、その条件に依ってサービスを無効化あるいは有効化します。

ヘルスチェックを有効化するには `app.config` の `riak_core` セクションに新しい設定を追加してください:

    %% Health Checks
    %% If disabled, health checks registered by an application will
    %% be ignored. NOTE: this option cannot be changed at runtime.
    %% To re-enable, the setting must be changed and the node restarted.
    %% ヘルスチェック
    %% 無効の場合、application が追加したヘルスチェックは無視されます。
    %% 注意: このオプションは実行中に変更できません。
    %% 有効化するには設定を変えた後にノード再起動が必要です。
    {enable_health_checks, true},

Riak は KV vnode のメッセージキュー長をモニターするヘルスチェックを登録します。kv ヘルスチェックを設定するには `app.config` の `riak_kv` セクションに新しい設定を追加します:

    %% This option configures the riak_kv health check that monitors
    %% message queue lengths of riak_kv vnodes. The value is a 2-tuple,
    %% {EnableThreshold, DisableThreshold}. If a riak_kv_vnode's message
    %% queue length reaches DisableThreshold the riak_kv service is disabled
    %% on this node. The service will not be re-enabled until the message queue
    %% length drops below EnableThreshold.
    %% このオプションで riak_kv vnode のメッセージキュー長のヘルスチェックを
    %% 設定します。値は 2-タプルで {有効化しきい値、無効化しきい値}です。
    %%  riak_kv vnode のメッセージキュー長が無効化しきい値に達すると riak_kv 
    %% サービスが無効化されます。有効化しきい値より下に値が下がるまでは
    %% サービスは再度有効化されません。

    {vnode_mailbox_limit, {1, 5000}}

注意: kv ヘルスチェックは Riak Search や Riak Pipe vnode には適用されません。

#### バケットプロパティのリセット

HTTP インターフェイスを通じて、バケットプロパティをデフォルト設定へリセット出来るようになりました。バケットプロパティは、クラスタ内のゴシッププロトコルにより、 Riak のリング構造体として保存されます。すでに使われていないバケットのプロパティのリセットや、デフォルト設定で使われているバケットのリセットによりゴシップで転送されるデータを削減できます。

#### syslog へのログ出力サポート

Riak 1.3 では syslog へのログ出力をサポートします。有効にするには riak の app.config で lager の下にある `handlers` セクションへ次のような設定を追加します:

```
{lager_syslog_backend, ["riak", daemon, info]}
```

この設定では、info レベル以上のすべてのメッセージを、 daemon ファシリティへ向けて、 ‘riak’ アイデンティティでログ出力します。さらなる情報は lager_syslog ドキュメントをご覧ください。

https://github.com/basho/lager_syslog

### Installation Notes
### インストールの注意点

RHEL/CentOS/Fedora ユーザは、RPM ツールが `expect` への依存を追加したため、次のようなメッセージが出た場合には:
```
$ sudo rpm -i riak-1.3.0rc1-1.el5.x86_64.rpm
error: Failed dependencies:
    /usr/bin/expect is needed by riak-1.3.0rc1-1.x86_64
```

Riak RPM を `yum` からインストールすることで依存性を自動解決できます:

```
$ sudo yum -y install riak-1.3.0rc1-1.el5.x86_64.rpm
Preparing...                ########################################### [100%]
   1:expect                 ########################################### [100%]
   2:riak                   ########################################### [100%]
```

### 解決された issues/ PRs

* riak: `ulimit -n` warning message bumped from 1024 to 4096
* riak/192: [Permissions not checked on $PIPE_DIR in riak script](https://github.com/basho/riak/issues/192)
* riak/266: [Add libstdc++ library to LD_PRELOAD path to find proper symbols in SmartOS](https://github.com/basho/riak/issues/266)
* riak/261: [Add 'riak-admin aae_status', plus add AAE options to app.config](https://github.com/basho/riak/issues/261)
* riak/259: [remove legacy-mapred-only configs](https://github.com/basho/riak/issues/259)
* riak/253: [Properly exit on `/etc/init.d/riak status` command](https://github.com/basho/riak/issues/253)
* riak/251: [`riak stop` does not behave properly on BSD systems](https://github.com/basho/riak/issues/251)
* riak/274: [Riak fails to start on a single CPU machine](https://github.com/basho/riak/issues/274)
* riak: [Set riak_sysmon's gc_ms_limit default value to zero](https://github.com/basho/riak/commit/065a2abf2ee3bbcd8da0fcf885f1fc8cd8f6327d)
* basho_stats/2: [update rebar to 2.0.0](https://github.com/basho/basho_stats/issues/2)
* bitcask/42: [Disable merges on startup to prevent high disk io with heavy requests](https://github.com/basho/bitcask/issues/42)
* bitcask/45: [remove arbitrary 120-char limit on log_needs_merge messages](https://github.com/basho/bitcask/issues/45)
* bitcask/46: [Support rebar binary in system](https://github.com/basho/bitcask/issues/46)
* bitcask/49: [update rebar to 2.0.0](https://github.com/basho/bitcask/issues/49)
* bitcask/54: [Adds "grace period" to stop just-written files from expiring.](https://github.com/basho/bitcask/issues/54)
* bitcask/55: [Change erlang:now() -> os:timestamp() when it is safe](https://github.com/basho/bitcask/issues/55)
* bitcask/56: [remove -author attributes from source](https://github.com/basho/bitcask/issues/56)
* bitcask/58: [merge process should write CRC to hintfiles](https://github.com/basho/bitcask/issues/58)
* bitcask/59: [Dss timeshift crc](https://github.com/basho/bitcask/issues/59)
* bitcask/65: [Iterator API](https://github.com/basho/bitcask/issues/65)
* bitcask/66: [Fix log spam introduced by branch 'gh62-badrecord-mstate'](https://github.com/basho/bitcask/issues/66)
* bitcask/67: [Add bitcask:is_empty_estimate](https://github.com/basho/bitcask/issues/67)
* bitcask/70: [Clear all Dialyzer warnings](https://github.com/basho/bitcask/issues/70)
* bitcask/76: [Make Bitcask I/O mode configurable: Erlang vs NIF](https://github.com/basho/bitcask/pull/76)
* bitcask/77: [Change default Bitcask I/O mode to Erlang](https://github.com/basho/bitcask/pull/77)
* cluster_info/11: [Remove Luke usage](https://github.com/basho/cluster_info/issues/11)
* cluster_info/8: [update rebar to 2.0.0](https://github.com/basho/cluster_info/issues/8)
* ebloom/8: [update rebar to 2.0.0](https://github.com/basho/ebloom/issues/8)
* leveldb/71: [convert LRUCache from LRU to simple fifo](https://github.com/basho/leveldb/pull/71)
* eleveldb/37: [raise bits per key from 10 to 16.  reduces false positive rate.](https://github.com/basho/eleveldb/issues/37)
* eleveldb/38: [updated to rebar 2.0.0](https://github.com/basho/eleveldb/issues/38)
* eleveldb/39: [Export iterator/3 so users can iterate over just keys](https://github.com/basho/eleveldb/issues/39)
* eleveldb/40: [make bloom2 the default bloom filter](https://github.com/basho/eleveldb/issues/40)
* eleveldb/42: [Add Erlang VM reduction count 'bumps' to all NIF calls](https://github.com/basho/eleveldb/issues/42)
* eleveldb/44: [Mv thread direct](https://github.com/basho/eleveldb/issues/44)
* eleveldb/45: [Jfw return value rodeo](https://github.com/basho/eleveldb/issues/45)
* eleveldb/48: [Jdb mv iterate5](https://github.com/basho/eleveldb/issues/48)
* eleveldb/49: [Mv unordered close](https://github.com/basho/eleveldb/issues/49)
* eleveldb/50: [Address race condition between queue asking for help and a single worker...](https://github.com/basho/eleveldb/issues/50)
* erlang_js/29: [update rebar to 2.0](https://github.com/basho/erlang_js/issues/29)
* erlang_js/30: [Dss fix ejslog](https://github.com/basho/erlang_js/issues/30)
* lager/53: [adding css for edocs; also adding edoc pointer to README](https://github.com/basho/lager/issues/53)
* lager/56: [Add support for a custom log truncation size compile time flag](https://github.com/basho/lager/issues/56)
* lager/67: [Added CRs to the LFs in lager_console_backend](https://github.com/basho/lager/issues/67)
* lager/68: [Direct the console logger output to user](https://github.com/basho/lager/issues/68)
* lager/69: [update rebar to 2.0.0](https://github.com/basho/lager/issues/69)
* lager/70: [Added lager:start() to the README](https://github.com/basho/lager/issues/70)
* lager/76: [Add informtion about loggly backend](https://github.com/basho/lager/issues/76)
* lager/77: [Use quickcheck to test for formatting equivalenve with io_lib](https://github.com/basho/lager/issues/77)
* lager_syslog/6: [Lager 2.0 support](https://github.com/basho/lager_syslog/issues/6)
* lager_syslog/7: [Use git:// rather than https:// so hosts don't need curl](https://github.com/basho/lager_syslog/issues/7)
* lager_syslog/8: [convert_level(?EMERGENCY) -> emergency. ](https://github.com/basho/lager_syslog/issues/8)
* merge_index/22: [update rebar to 2.0.0](https://github.com/basho/merge_index/issues/22)
* merge_index/25: [Change erlang:now() -> os:timestamp() when it is safe](https://github.com/basho/merge_index/issues/25)
* mochiweb/4: [erlang:now() -> os:timestamp](https://github.com/basho/mochiweb/issues/4)
* erlang_protobuffs/26: [Issue 25](https://github.com/basho/erlang_protobuffs/issues/26)
* erlang_protobuffs/34: [Fixed defaults for decodes to handle camelCase feilds.](https://github.com/basho/erlang_protobuffs/issues/34)
* erlang_protobuffs/36: [Fixed warnings about unused functions and variables](https://github.com/basho/erlang_protobuffs/issues/36)
* erlang_protobuffs/38: [Performance improvements](https://github.com/basho/erlang_protobuffs/issues/38)
* riak_api/10: [Add deregistration of services](https://github.com/basho/riak_api/issues/10)
* riak_api/12: [Restore stat mod registration](https://github.com/basho/riak_api/issues/12)
* riak_api/14: [Allow multiple replies to be sent in the middle of a streaming operation.](https://github.com/basho/riak_api/issues/14)
* riak_api/17: [Use riak_api_pb_sup active children count for pb active stat](https://github.com/basho/riak_api/issues/17)
* riak_api/18: [Use folsom's `gauge` type to store the function needed to pbcconnects ](https://github.com/basho/riak_api/issues/18)
* riak_api/19: [Performance improvements](https://github.com/basho/riak_api/issues/19)
* riak_control/26: [Typo fix (no such function - gen_server:cast/3)](https://github.com/basho/riak_control/issues/26)
* riak_control/27: [Re-export admin_ring:node_ring_details/2 back](https://github.com/basho/riak_control/issues/27)
* riak_control/37: [Convert to Ember.js.](https://github.com/basho/riak_control/issues/37)
* riak_control/38: [update rebar to 2.0.0](https://github.com/basho/riak_control/issues/38)
* riak_control/40: [Deprecate unused resources.](https://github.com/basho/riak_control/issues/40)
* riak_control/42: [Add new Riak Control theme.](https://github.com/basho/riak_control/issues/42)
* riak_control/44: [Do not require secure only cookie.](https://github.com/basho/riak_control/issues/44)
* riak_control/48: [Move formatting functions to riak_control_formatting.](https://github.com/basho/riak_control/issues/48)
* riak_control/53: [style.css file missing from some packages](https://github.com/basho/riak_control/pull/53)
* riak_core/137: [Change node to use claim_v1 when in legacy mode](https://github.com/basho/riak_core/issues/137)
* riak_core/188: [Eunit cleanups](https://github.com/basho/riak_core/issues/188)
* riak_core/195: [Change write_ringfile to create a temporary ring file, check and rename.](https://github.com/basho/riak_core/issues/195)
* riak_core/220: [update rebar to 2.0.0](https://github.com/basho/riak_core/issues/220)
* riak_core/223: [Change ticks from timer to more efficient erlang:send_after](https://github.com/basho/riak_core/issues/223)
* riak_core/224: [erlang:now() -> os:timestamp() in all the places it is safe](https://github.com/basho/riak_core/issues/224)
* riak_core/225: [remove -author attributes from source](https://github.com/basho/riak_core/issues/225)
* riak_core/230: [Remove publish_capabilities race](https://github.com/basho/riak_core/issues/230)
* riak_core/232: [Address high memory use by riak_core_sysmon_handler](https://github.com/basho/riak_core/issues/232)
* riak_core/235: [No open source license specified](https://github.com/basho/riak_core/issues/235)
* riak_core/236: [adding license file, closes #235](https://github.com/basho/riak_core/issues/236)
* riak_core/240: [health check system](https://github.com/basho/riak_core/issues/240)
* riak_core/246: [Vnode shutdown message severity should be info](https://github.com/basho/riak_core/issues/246)
* riak_core/249: [Allow gen_nb_server to use IPv6 addresses.](https://github.com/basho/riak_core/issues/249)
* riak_core/250: [Make vnode check for existing handoff before starting another](https://github.com/basho/riak_core/issues/250)
* riak_core/251: [rewriting revised readme in .md and removing .org version.](https://github.com/basho/riak_core/issues/251)
* riak_core/254: [Export path and stat_name types from riak_core_stat_q](https://github.com/basho/riak_core/issues/254)
* riak_core/255: [upgrade legacy ring only if needed](https://github.com/basho/riak_core/issues/255)
* riak_core/257: [Enable riak_core apps to provide a health_check callback](https://github.com/basho/riak_core/issues/257)
* riak_core/259: [Adjust riak_core_sup child order for cleaner shutdown](https://github.com/basho/riak_core/issues/259)
* riak_core/261: [Fix bug in riak_core_format:human_time + add test](https://github.com/basho/riak_core/issues/261)
* riak_core/262: [Add ability to selectively disable incoming/outgoing handoff](https://github.com/basho/riak_core/issues/262)
* riak_core/264: [Make vnode terminate backend for any exit reason](https://github.com/basho/riak_core/issues/264)
* riak_core/265: [Fix bug in riak_core_util:rpc_every_member_ann](https://github.com/basho/riak_core/issues/265)
* riak_core/268: [Fix riak_core_wm_urlmap](https://github.com/basho/riak_core/issues/268)
* riak_kv/290: [timeout/forward_preflist MapReduce error.](https://github.com/basho/riak_kv/issues/290)
* riak_kv/408: ["fitting was gone before startup" errors.](https://github.com/basho/riak_kv/pull/408)
* riak_kv/354: [Add PB service deregistration on stop.](https://github.com/basho/riak_kv/issues/354)
* riak_kv/360: [Fix eunit failures](https://github.com/basho/riak_kv/issues/360)
* riak_kv/366: [Spurious #pipe_log messages in logs as "Unrecognized message"](https://github.com/basho/riak_kv/issues/366)
* riak_kv/367: [riak 1.2rc1 - memory backend issue with 2i & $key/$bucket](https://github.com/basho/riak_kv/issues/367)
* riak_kv/379: [Resolve 2I timeout error from case clause](https://github.com/basho/riak_kv/issues/379)
* riak_kv/380: [Improper match on w_val_unsatisfied error in HTTP](https://github.com/basho/riak_kv/issues/380)
* riak_kv/382: [Use regular logging on Travis for now so we can actually read the output](https://github.com/basho/riak_kv/issues/382)
* riak_kv/390: [erlang:now() -> os:timestamp() in all the places it is safe](https://github.com/basho/riak_kv/issues/390)
* riak_kv/395: [Add retry on eleveldb lock errors during open for up to 1 minute.](https://github.com/basho/riak_kv/issues/395)
* riak_kv/399: [2I backpressure](https://github.com/basho/riak_kv/issues/399)
* riak_kv/401: [Quick fix to prevent mapred_test from hanging](https://github.com/basho/riak_kv/issues/401)
* riak_kv/404: [Spawn remote vnodes using start_link rather than start.](https://github.com/basho/riak_kv/issues/404)
* riak_kv/405: [Randoming kv_put forwardee node](https://github.com/basho/riak_kv/issues/405)
* riak_kv/406: [Make riak_client work on non-riak nodes after get/put FSM startup change](https://github.com/basho/riak_kv/issues/406)
* riak_kv/415: [Fix stats for r15b02](https://github.com/basho/riak_kv/issues/415)
* riak_kv/419: [add knob to disable referer check](https://github.com/basho/riak_kv/issues/419)
* riak_kv/423: [Riak KV vnodes can block in certain scenarios when using Bitcask](https://github.com/basho/riak_kv/issues/423)
* riak_kv/424: [Change riak_kv_bitcask_backend to use bitcask:is_empty_estimate](https://github.com/basho/riak_kv/issues/424)
* riak_kv/426: [Since "Try again" is not relevant to a self-join, make that error explicit](https://github.com/basho/riak_kv/issues/426)
* riak_kv/429: [Apply backpressure from the MR sink](https://github.com/basho/riak_kv/issues/429)
* riak_kv/433: [Remove Luke usage](https://github.com/basho/riak_kv/issues/433)
* riak_kv/435: [remove the unused 'mget' command from riak_kv_vnode](https://github.com/basho/riak_kv/issues/435)
* riak_kv/438: [get put stats -> 1.2-perf](https://github.com/basho/riak_kv/issues/438)
* riak_kv/439: [Fixing Dialyzer Complaints Near MapReduce Code](https://github.com/basho/riak_kv/issues/439)
* riak_kv/440: [Re-instate code from #415 that was lost in later changes](https://github.com/basho/riak_kv/issues/440)
* riak_kv/442: [Merge 1.2-perf into master](https://github.com/basho/riak_kv/issues/442)
* riak_kv/447: [Add basic health check to Riak KV](https://github.com/basho/riak_kv/issues/447)
* riak_kv/449: [Update stats in process](https://github.com/basho/riak_kv/issues/449)
* riak_kv/451: [Change AAE hashtree to buffer and batch write to LevelDB](https://github.com/basho/riak_kv/issues/451)
* riak_kv/453: [Avoid a badarg in crypto:rand_uniform when N=1](https://github.com/basho/riak_kv/issues/453)
* riak_kv/454: [Try to make KV shutdown cleaner](https://github.com/basho/riak_kv/issues/454)
* riak_kv/456: [Add AAE status subsystem + finalize AAE for Riak 1.3 release](https://github.com/basho/riak_kv/issues/456)
* riak_kv/457: [Correct usage of capabilities API in riak_kv_pb_object.](https://github.com/basho/riak_kv/issues/457)
* riak_kv/458: [Make index_hashtree process exit when related vnode exits](https://github.com/basho/riak_kv/issues/458)
* riak_kv/459: [Fix health check code to handle dead pids](https://github.com/basho/riak_kv/issues/459)
* riak_kv/460: [Fix AAE exchange bug for the N=1 case](https://github.com/basho/riak_kv/issues/460)
* riak_kv/476: [Take node liveness into account during contant hash choice for reduce phase](https://github.com/basho/riak_kv/issues/476) - Thanks Gunin Alexander
* riak_kv/478: [Improve interaction between AAE and K/V deletion](https://github.com/basho/riak_kv/pull/478)
* riak_kv/482: [Randomize the LevelDB write_buffer_size used for AAE](https://github.com/basho/riak_kv/pull/482)
* riak_kv/483: [Improve AAE backpressure on K/V vnode write-path](https://github.com/basho/riak_kv/pull/483)
* riak_kv/486: [Make AAE default to 'off' when not configured](https://github.com/basho/riak_kv/pull/486)
* riak_pb/15: [Maven build](https://github.com/basho/riak_pb/issues/15)
* riak_pb/18: [Add proto_cmd to MANIFEST.in](https://github.com/basho/riak_pb/issues/18)
* riak_pb/19: [Add OSGi Manifest headers to riak-pb jar file](https://github.com/basho/riak_pb/issues/19)
* riak_pb/25: [Java POM changes for OSGI](https://github.com/basho/riak_pb/issues/25)
* riak_pb/26: [Fix bug with protobuffs encoding tests](https://github.com/basho/riak_pb/issues/26)
* riak_pb/27: [Add protobuf to install_requires.](https://github.com/basho/riak_pb/issues/27)
* riak_pb/29: [Pin python package version and bump it.](https://github.com/basho/riak_pb/issues/29)
* riak_pipe/52: [update to rebar 2.0.0](https://github.com/basho/riak_pipe/issues/52)
* riak_pipe/53: [erlang:now() -> os:timestamp()](https://github.com/basho/riak_pipe/issues/53)
* riak_pipe/59: ["Sink type" that can provide backpressure](https://github.com/basho/riak_pipe/issues/59)
* riak_pipe/61: [Clean up some dialyzer warnings](https://github.com/basho/riak_pipe/issues/61)
* riak_pipe/65: [Quickchecking riak_pipe_fitting](https://github.com/basho/riak_pipe/issues/65)
* riak_search/116: [Add PB service deregistration on stop.](https://github.com/basho/riak_search/issues/116)
* riak_search/127: [update rebar to 2.0.0](https://github.com/basho/riak_search/issues/127)
* riak_search/128: [Fix mis-reporting of fl=ID + sort field on PBC.](https://github.com/basho/riak_search/issues/128)
* riak_search/129: [erlang:now() -> os:timestamp() when it is safe](https://github.com/basho/riak_search/issues/129)
* riak_search/133: [Remove Luke usage](https://github.com/basho/riak_search/issues/133)
* riak_sysmon/7: [update rebar to 2.0.0](https://github.com/basho/riak_sysmon/issues/7)
* riaknostic/39: [Added some reassuring output.](https://github.com/basho/riaknostic/issues/39)
* riaknostic/41: [added a first pass at machine-readable output](https://github.com/basho/riaknostic/issues/41)
* riaknostic/50: [Export command](https://github.com/basho/riaknostic/issues/50)
* riaknostic/51: [Pevm sysctl checks](https://github.com/basho/riaknostic/issues/51)
* riaknostic/52: [Update README.md](https://github.com/basho/riaknostic/issues/52)
* webmachine/101: [pass method,scheme,http vsn into rewrite](https://github.com/basho/webmachine/issues/101)
* webmachine/102: [Update demo app: rebar deps, ensure inets is running, improve README](https://github.com/basho/webmachine/issues/102)
* webmachine/106: [Store the dispatch_list in ETS, not application:set_env](https://github.com/basho/webmachine/issues/106)
* webmachine/113: [Fix setup/teardown for etag EQC test](https://github.com/basho/webmachine/issues/113)
* webmachine/56: [Strip whitespace from content-type (and others) parsed by webmachine_uti...](https://github.com/basho/webmachine/issues/56)
* webmachine/65: [swap - to _ for app name.](https://github.com/basho/webmachine/issues/65)
* webmachine/73: [Fix for trailing CRLF ](https://github.com/basho/webmachine/issues/73)
* webmachine/75: [Exposed get_routes() to the public.](https://github.com/basho/webmachine/issues/75)
* webmachine/77: [Incorrect accept header in some blackberry devices.](https://github.com/basho/webmachine/issues/77)
* webmachine/81: [remove code:clash call](https://github.com/basho/webmachine/issues/81)
* webmachine/83: [update rebar to 2.0.0](https://github.com/basho/webmachine/issues/83)
* webmachine/86: [Bump WMVSN to 1.9.2.](https://github.com/basho/webmachine/issues/86)
* webmachine/93: [change parameterized modules to regular modules](https://github.com/basho/webmachine/issues/93)
* webmachine/97: [Header Rewriting](https://github.com/basho/webmachine/issues/97)
