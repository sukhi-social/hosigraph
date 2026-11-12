# dlmdb で見つけた 2 つのこと

Takka を作る途中で、`datalevin/dlmdb` の挙動に 2 つ引っかかりました。
どちらも **純粋な C だけで再現できます**(Haskell のバインディングのせいではありません)。
再現コードは [`docs/repro/`](repro/) にあります。

調べたバージョン: `VENDORED_FROM.txt` を参照(`cbits/dlmdb/` に取り込んだコミット)。

---

## 1. `MDB_PREFIX_COMPRESSION` でキーが消える(データ破損)

### 症状

プレフィックス圧縮を付けた DBI に、キーを次の順で入れるだけです。

```
put [2]   put [0,2]   put [0,1,1]
```

| | 結果 |
|---|---|
| 期待する走査 | `[0 1 1]` `[0 2]` `[2]` |
| 実際の走査 | `[0 1 1]` `[0 2]` `[0 2]` |
| `mdb_get([2])` | `MDB_NOTFOUND` |

つまりキー `[2]` が失われて、`[0,2]` が 2 回見えます。エラーは出ません。

### 再現

```sh
cc -I cbits/dlmdb docs/repro/prefix_bug.c cbits/dlmdb/mdb.c cbits/dlmdb/midl.c -lpthread -o prefix_bug
./prefix_bug
```

### 切り分け(ファズ:ランダム順に挿入 → ランダムに削除 → 走査・件数・rank・get を集合モデルと照合)

実行: `scripts/build-ghc.sh fuzz && build/dlmdb-fuzz`(出典: [`bench/FuzzDlmdb.hs`](../bench/FuzzDlmdb.hs))

| キーの形 | 設定 | 壊れた試行 |
|---|---|---|
| 一般的なキー(先頭バイトがばらばら、`0x00` を含む) | プレフィックス圧縮のみ | **32 / 200**(30 キー)・**12 / 60**(2000 キー) |
| 同上 | COUNTED + プレフィックス圧縮 | **40 / 200** |
| ゼロバイトを含まないキー | プレフィックス圧縮のみ | 0 / 200 |
| 先頭バイトが全部同じ(`[7, …]`) | プレフィックス圧縮のみ | 0 / 200 |
| Takka の AVE 風(`00 00 00 aid …`) | プレフィックス圧縮のみ | 0 / 200・0 / 60(2000 キー) |
| Takka の EAV 風(`80 00 … 01 …`) | プレフィックス圧縮のみ | 0 / 200 |
| Takka の AVE / EAV 風 | COUNTED + プレフィックス圧縮 | 0 / 200・0 / 200・0 / 60 |
| 一般的 / AVE 風 / EAV 風 | COUNTED のみ | 0(200 試行 × 3 と 60 試行 × 3) |
| 一般的なキー | 何も付けない | 0 / 200 |

* `MDB_COUNTED` なしの「プレフィックス圧縮だけ」でも壊れます(= COUNTED のせいではない)。
* 引き金は「**隣り合うキーの共有プレフィックス長がゼロ**になる並びを、途中に挿入したとき」のようです
  (私の推測です。原因箇所までは特定していません)。
* ソート済みの順に入れた場合は壊れませんでした。

### Takka の対応

* プレフィックス圧縮は **オプトイン(デフォルトOFF)**: `ConnConfig { ccPrefixCompression = True }`。
* Takka のキーは先頭バイトが必ず揃います(EAV は `0x80`、AVE は `0x00 0x00`)。
  そのため上の表のとおり(Takka 風のキーでは合計 460 試行で)壊れませんでしたが、**証明ではなく経験則**です。
* 効果は大きいです(60 万 datoms で **約 36% 小さい**: 98 → 63 バイト/datom)。
  上流が直ったら、デフォルトに戻したいところです。
* `test/Spec.hs` に **カナリアテスト**があります。バグが再現しなくなると
  `NOTE the dlmdb prefix-compression bug no longer reproduces` と知らせます。
* DBI のフラグは「作るとき」にだけ効き、開き直すと保存済みのフラグが使われます
  (私の実験では、違うフラグを渡してもエラーにならず無視されました)。

### 上流への issue 案(英語)

> **MDB_PREFIX_COMPRESSION loses a key / returns a duplicate after out-of-order inserts**
>
> With a DBI opened with `MDB_CREATE | MDB_PREFIX_COMPRESSION` (no `MDB_COUNTED` needed),
> inserting the keys `{2}`, `{0,2}`, `{0,1,1}` in this order in a single write transaction
> results in a cursor scan of `{0,1,1} {0,2} {0,2}`; `mdb_get({2})` returns `MDB_NOTFOUND`.
> Inserting the same keys in sorted order, or keys without a zero byte, or a plain DBI, is fine.
> A small fuzzer (random inserts then random deletes, compared against an ordered set)
> shows corruption only for key sets where adjacent keys can share a zero-length prefix.
> Reproducer: `prefix_bug.c` (pure C, ~40 lines).

---

## 2. 新しい読み取りトランザクションの最初の `mdb_count_range` が 0 を返す

### 症状

読み取りトランザクションを開いた直後は、名前付き DBI のメタデータが「古い(`DB_STALE`)」状態です。
`mdb_count_all` や `mdb_get_rank`、カーソル系はそれを更新しますが、
`mdb_count_range` は更新する **前** に `md_entries` を読んで、空だと判断して 0 を返します。
`mdb_count_all` を一度呼ぶと、その後は正しい値になります。

```
1st mdb_count_range([b,e)) = 0   (expected 3)
mdb_count_all              = 5
2nd mdb_count_range([b,e)) = 3   (expected 3)
```

### 再現

```sh
cc -I cbits/dlmdb docs/repro/count_range_bug.c cbits/dlmdb/mdb.c cbits/dlmdb/midl.c -lpthread -o count_range_bug
./count_range_bug
```

### Takka の対応

`Takka.KV.countRange` が、数える前に軽い `mdb_count_all` を一度呼んで更新を済ませます
(2 回目以降はフラグを見るだけです)。ソース `cbits/dlmdb/` には手を入れていません。
`test/Spec.hs` の「range counts, ranks … (counted DBI)」が、
新しいトランザクションでの最初の呼び出しを毎回検査します。

この件で最初に気づいたきっかけは、クエリプランナーの見積もりが `est=0.0` になったことでした。

### 上流への issue 案(英語)

> **mdb_count_range returns 0 on the first call in a fresh read transaction**
>
> `mdb_count_range` reads `txn->mt_dbs[dbi].md_entries` before the named DB's record has been
> refreshed (`DB_STALE`), so on a fresh read txn it sees the stale (empty) value and returns 0.
> `mdb_count_all` refreshes first and works; calling it before `mdb_count_range` makes
> `mdb_count_range` correct. Reproducer: `count_range_bug.c`.

---

## 補足: 大きな値(オーバーフローページ)と COUNTED

Takka は 1MB までの文字列を索引エントリの「値」として入れます。COUNTED な DBI で、
200 件の挿入・66 件の削除のあとの件数・rank・走査の一致、1MB の値の往復を `test/Spec.hs` で確認していて、
問題は見つかっていません(プレフィックス圧縮ありでも同様でした)。

## 補足: count_range の境界の意味(実験で確認)

`mdb_count_range(txn, dbi, low, high, flags, &n)`:

* `flags = 0` は **両端とも開区間**。
* `MDB_COUNT_LOWER_INCL (0x02)` で下端を含める。プレフィックス範囲 `[prefix, successor)` はこれ。
* `low` / `high` に `NULL` を渡すと、その側は無限。
* `MDB_SET_RANGE` に空のキーを渡すと `MDB_BAD_VALSIZE`(先頭からは `MDB_FIRST` を使う)。
* キーの最大長は 511 バイト。
