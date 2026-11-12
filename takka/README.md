# Takka

Haskell で書いた、小さくて組み込みやすい **Datalog データベース**です。
[Datalevin](https://github.com/datalevin/datalevin) の使い心地に憧れて、
その土台の [dlmdb](https://github.com/datalevin/dlmdb)(LMDB のフォーク)の上に、
Haskell で一から組み直してみた試作品(v0.1)です。

名前の takka は、パーリ語で「推論・論理」(サンスクリットの tarka と同じ根)。Datalog の logic から取りました。
もとの名前は Hevin でした。いまは [hosigraph](../README.md) の repo の中の `takka/` にあります。

* 永続・ACID・読み取りが書き込みを止めない(MVCC)
* EAV のトリプルストア。JSON-LD の「ノード → エンティティ、プロパティ → 属性」と相性がいい
* EDN の Datalog:パターン・述語・関数・`not` / `not-join`・`or` / `or-join`・集計・**再帰ルール**
* **pull**(入れ子・逆参照・再帰・`:default` / `:as` / `limit`、`:find` の中の `(pull ?e […])` も)
* 索引の**件数**から結合の順番を決めるプランナー(dlmdb の数え上げ木を使います)
* 範囲述語(`[(> ?a 97)]`)は **AVE の範囲走査**になる。大きな値(MB 単位の文字列)も**そのまま**入る
* `cabal build` だけで動く(dlmdb の C ソースは同梱。システムの LMDB は不要)

> Datalevin 本体(Clojure)のコードは読んでいません。公開されている使い方の「形」を参考にした、
> 別実装です(clean-room)。土台の dlmdb は C のソースを読んで使っています。
> Datalevin の機能との網羅的な比較もしていません。

## 5 分で試す

```sh
cabal build all
cabal run takka-demo          # ひとめぐりのデモ
cabal test                    # 76 個のテスト(QuickCheck 含む)
```

cabal が Hackage に届かない環境でも、`ghc` と `gcc` があれば:

```sh
scripts/build-ghc.sh app && build/takka-demo
scripts/build-ghc.sh test && build/takka-test
scripts/build-ghc.sh bench && build/takka-bench
```

## 使ってみる

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Takka
import qualified Data.Map as M

main :: IO ()
main = withConn "/tmp/mydb" schema $ \conn -> do
  _ <- transactEdn conn
    "[{:db/id -1 :person/name \"Frege\"  :person/age 70 :person/friend [-2]} \
    \ {:db/id -2 :person/name \"Peirce\" :person/age 75}]"
  rows <- q conn
    "[:find ?n ?f :where [?e :person/name ?n] [?e :person/friend ?x] [?x :person/name ?f]]" []
  print rows            -- [[VStr "Frege",VStr "Peirce"]]
  where
    schema = M.fromList
      [ ("person/name",   identity (attr TString))   -- 一意 → 同じ名前なら upsert
      , ("person/age",    attr TLong)
      , ("person/friend", many ref) ]                -- 参照(複数)
```

もう少し(`demo` から):

```clojure
;; 集計
[:find (avg ?a) (max ?a) (count ?e) :where [?e :person/age ?a]]

;; 否定
[:find ?n :where [?e :person/name ?n] (not [_ :person/friend ?e])]

;; 再帰ルール(到達可能性)  — q conn "..." [InRules rules]
[[(reach ?a ?b) [?a :person/friend ?b]]
 [(reach ?a ?b) [?a :person/friend ?c] (reach ?c ?b)]]
[:find ?from ?to :in $ % :where (reach ?x ?y) [?x :person/name ?from] [?y :person/name ?to]]
```

```clojure
;; 範囲述語:age の索引の該当部分だけを読む(`(< 2 ?a 5)` の連鎖や、入力変数 ?min も可)
[:find ?n :where [?e :person/age ?a] [(> ?a 97)] [?e :person/name ?n]]

;; or-join:分岐の中だけの変数(?a)は外と混ざらない
[:find ?n :where [?e :person/name ?n]
                 (or-join [?e] [?e :person/alias "G. Frege"] (and [?e :person/age ?a] [(> ?a 70)]))]

;; pull(qCells で受け取る。単独の pull / pullEdn もある)
[:find (pull ?e [:person/name (:person/age :default 0) {:person/friend [:person/name]} :person/_friend])
 :where [?e :person/name "De Morgan"]]
;; => {:person/name "De Morgan", :person/age 65, :person/friend [{:person/name "Peirce"}], :person/_friend [{:db/id 4}]}
```

プランナーが何を選んだか見たいときは `explainEdn`:

```
  [?e :person/age ?a]   est=3.0   [range scan]   rows 1 -> 3
  [(> ?a 60)]                                    rows 3 -> 3
  [?e :person/name ?n]  est=1.0                  rows 3 -> 3
```

### JSON-LD

```haskell
_   <- importJsonLD conn doc                      -- aeson の Value
out <- withDb conn $ \db ->
         exportByIri db defaultExportOptions { eoContext = M.fromList [("schema","http://schema.org/")] }
                     ["http://example.org/alice"]
```

* `@id` → 属性 `:jsonld/id`(一意なので、同じ文書を何度入れても重複しません)
* `@type` → `:jsonld/type`、プロパティ → **展開済み IRI を名前にした属性**(多値)
* 入れ子のノード / `{"@id": …}` / `"@type": "@id"` の項 → 参照
* 対応: `@vocab`、項の定義、`schema:name` 形式の compact IRI、context の配列、ノードごとの context、`@graph`
* 未対応(黙って無視せず**エラーにします**): リモート context、`@list`、`@reverse`、`@nest`、フレーミング、
  JSON-LD 1.1 の展開・圧縮アルゴリズム全体。リテラルの `@type` / `@language` は捨てます。

## つくり

```
 Takka            facade(q / transactEdn / withDb / importJsonLD …)
 ├─ JsonLD        JSON-LD ⇄ tx-data / entities
 ├─ Query         EDN Datalog:パーサ → プランナー → 実行、ルール(層別 semi-naive)
 ├─ Pull          pull パターンの読み取りと実行、EDN / JSON への出力
 ├─ TxEdn, Edn    EDN リーダ、tx-data の読み取り
 ├─ Store         トリプルストア:スキーマ・トランザクション・upsert・ユニーク制約・見積もり
 ├─ Codec         順序を保つバイト列エンコード(バイト比較 = 値の比較)
 ├─ KV            括弧付きで安全な dlmdb ラッパ(txn・カーソル・数え上げ・rank)
 └─ Native        FFI(cbits/dlmdb/ の C ソースに直接)
```

* 索引は 2 本: `EAV = e ++ attr ++ value`、`AVE = attr ++ value ++ e`。
  どちらも COUNTED な DBI なので、**キー接頭辞の件数が O(log n)** で取れます。
* **大きな値**: エンコードが 400 バイトを超える値は、キーには固定長の代役
  (`0x60 ++ 元の型タグ ++ MD5`、18 バイト)だけを入れ、**本体は索引エントリの「値」側**に置きます。
  別テーブルも参照カウントも要らず、エントリを消せば本体も消えます。書くときに本体を比べるので、
  ハッシュが衝突しても黙って上書きしません。等値検索は小さな値と同じ速さで効きます。
* 値のエンコードは「並びを保ち、後ろに何が続いても区切れる」形なので、
  接頭辞の範囲走査がそのまま索引検索になります。
* **範囲述語**: 述語(`< <= > >= =`、`(< 2 ?a 5)` の連鎖、定数・束縛済み変数どちらも)から変数の
  上下限を読み取り、AVE のキー範囲だけを走査します。範囲は**絞り込みのための最適化**で、述語自体は
  消さずに残すので、広めに取っても結果は変わりません(int / double / 文字列 / キーワードを扱い、
  混ざった型は両方の領域を見ます)。範囲の件数(`countInRange`)はプランナーの見積もりにも使います。
* プランナーは、定数だけで決まる範囲の**件数**(`countDatoms`)と、結合する値の**ばらつき**
  (`distinctEstimate`:AVE から rank で等間隔に標本を取り、Good–Turing 風に推定)で、
  次に処理する句を貪欲に選びます。フィルタは変数が揃った時点ですぐ実行します。
* ルールは依存関係を強連結成分に分け、依存先から順に評価(層別)。成分の中は semi-naive 評価。
  同じ成分の中で `not` / `not-join` が再帰していたら、意味が定まらないのでエラーにします。
  ルール本体の `or` は分岐ごとのルールに**展開**してから評価するので、`or` の中の再帰呼び出しも
  semi-naive で正しく扱えます(分岐ローカルの変数は名前を付け替えて衝突を避けます)。
* `or` は、共有する変数がすべて束縛済みならフィルタとして早めに、そうでなければ(自分で変数を
  束縛するなら)ジェネレータとして最後に近い順で評価します。
* 書き込みは `MVar` で直列化し、スレッド版 RTS では bound thread で実行します
  (LMDB の書き込みトランザクションは OS スレッドに紐づくため)。
* `Db` は読み取りトランザクションそのもの(スナップショット)。`withDb` の中だけで有効です。

## 作りながら見つけたこと

正直に書いておきます。詳しくは [`docs/dlmdb-findings.md`](docs/dlmdb-findings.md)。

1. **dlmdb の `MDB_PREFIX_COMPRESSION` で、キーが消えることがあります**(純 C で再現済み)。
   ファズでは、先頭バイトがばらばらでゼロバイトを含むキーで壊れ、Takka のキー(先頭が揃う)や
   COUNTED だけの DBI では壊れませんでした。確実さを優先して **デフォルトOFF**にしています
   (`ConnConfig { ccPrefixCompression = True }` で有効。ファイルは約 36% 小さくなります)。
   カナリアテストが、上流で直ったら教えてくれます。
2. **新しい読み取りトランザクションの最初の `mdb_count_range` が 0 を返します**。
   `KV.countRange` が先に軽い `mdb_count_all` を呼んで回避しています。
3. 自分の実装の穴も見つけて直しました:ルール本体の `not` が未完成のルールを見てしまう問題
   (→ 層別化)、結果が 0 件のとき `:find` の未束縛変数を見逃す問題(→ 静的検査)、
   グループ化の左ネスト `++` による二乗の遅さ(→ 修正と、結果を返す前の強制評価)。
   どれも再発防止のテストがあります。
4. 大きな値を入れたら最初は 50KB の書き込みが 7.5ms もかかりました。測ってみると、`B.concatMap` による
   1 バイトずつのエスケープ(NUL がなくても!)と、デコードのたびの「ハッシュ確認」の再エンコードが犯人でした
   (MD5 自体は無実)。直して **27〜220 倍**速くなっています。当て推量で直さず、層ごとに測ったのが効きました。

## 数字(1 回きりの計測、目安として)

サンドボックス上、10 万人 × 3 属性 = 30 万 datoms、`build/takka-bench`:

| | |
|---|---|
| 一括ロード(10 トランザクション) | 約 **287,000 datoms/秒** |
| 主キー検索(名前 → 年齢、事前パース済み) | 約 **6.1 µs/クエリ** |
| 名前検索 → pull(3 属性) | 約 **5.6 µs** |
| 選択的な 3 句の結合 | 約 3 ms(500 行) |
| **範囲述語** `age > 98`(1%) | **0.004 秒**(範囲走査) ↔ 範囲が使えない同条件 0.166 秒 |
| `or` で 2 都市(2.5 万人) | 約 0.03 秒 |
| 10 万人の都市別集計 | 約 0.2 秒 |
| 件数の取得(`countDatoms`) | 約 0.6 µs |
| **50KB の文字列** 400 個(20MB)を 1 トランザクションで | 約 0.12 秒(約 170MB/秒) |
| 50KB の値そのものでの検索 | 約 150 µs |
| fsync ありの単発コミット | 約 2,600 commits/秒(ディスク次第) |
| 再帰ルール:長さ 800 のチェーンで 32 万ペア | 約 1.6 秒 |

ファイルサイズは 60 万 datoms で、COUNTED のみが 1 datom あたり約 98 バイト、
プレフィックス圧縮ありが約 63 バイトでした。

## 制限(いまの正直なところ)

* **大きな値**は入りますが、索引の中の並びは**内容ではなくハッシュ順**です。そのため文字列・キーワードの
  範囲述語は、大きな値の領域を**毎回全部**見ます(取りこぼさないため)。大きな値が大量にある属性では、
  文字列の範囲述語は速くなりません。また本体は EAV と AVE の両方に入るので、容量は 2 倍かかります。
* 範囲走査になるのは、**属性が定数のパターンで、値の変数に直接かかる比較**(`<` `<=` `>` `>=` `=`)です。
  `(+ ?a 0)` のような関数の結果への比較、二つの未束縛変数どうしの比較、真偽値の比較、
  そして `or` / `not` の**外側**にある述語が内側で束縛される変数にかかる場合は、
  従来どおり読んでからフィルタします(結果は同じで、遅いだけです)。
  同じ分岐の中のパターンと述語には、範囲走査が効きます。
* ルールは**全体を計算してから**使います(magic sets なし)。束縛した引数があっても関係全体を作ります。
* pull:`(limit :attr n)` を付けない多値属性は全部返します(Datomic の既定の 1000 件制限はありません)。
  `:find` のパターンを入力変数で渡すことや、コンポーネント属性の自動展開は未実装です。
* 未実装:複数ソース、`ref` の逆参照(`_attr`)をクエリのパターンで使うこと(pull では使えます)、
  component、全文検索・ベクトル検索、履歴、日時・UUID・バイト列・巨大整数の型、HA / レプリケーション。
* Linux で確認しています。macOS は動くはずですが未確認、Windows は未確認です。
* 書き込みは単一ライタ(LMDB の性質)。読み取りは並列です。
* マップ形式 tx-data で `[:attr x]` はちょうど 2 要素でルックアップ参照と解釈されます
  (キーワード値の多値 `[:a :b]` は多値として読みます)。

## 次の一歩の候補

* Elixir から使う:標準入出力の 1 行 1 リクエスト(EDN / JSON)サーバー + `Port`
* 大きな値の内容順(先頭バイトの断片をスタンドインに混ぜて、文字列範囲を大きな値でも効かせる)
* 二つの変数の比較や関数結果への範囲、magic sets、`:find` の pull パターンを入力で渡す
* プレフィックス圧縮を、上流の修正後にデフォルトへ

## ライセンス

* `cbits/dlmdb/` は [OpenLDAP Public License](cbits/dlmdb/LICENSE)(`COPYRIGHT`、`VENDORED_FROM.txt` も同梱)。
  取り込んだコミットは `VENDORED_FROM.txt` を見てください。
* Takka 本体のライセンスは**未設定**です(決めたら `takka.cabal` と `LICENSE` を足してね)。

## hosigraph から使われています

[hosigraph](../README.md)(出来事を本当のこととして追記し、「いま」を畳み込んで持つプロトタイプ)は、
Takka を次の 2 つの形で使います。Takka のほうは hosigraph を知りません。

* **「いま」を持つ小さなエンジン**として。hosigraph の `KhanaDatalevinNative` が、これです。
  Datalevin の jar や `java` なしで動くので、テストの基準(オラクル)と手元の試しに使っています。
  名前は、将来 Datalevin の Rust コアが出たときの「Datalevin Native の口」のために取ってあるもので、
  **いまの中身は Datalevin ではなく、この Takka です**。Rust コアが出たら、hosigraph はそちらを使う予定です(まだありません)。
* **ことば(型・EDN・Datalog の構文)の置き場所**として。`Takka.Types` / `Takka.Edn` / `Takka.Query` / `Takka.Pull` です。

「いま」を持つエンジンの口は、`Takka.Engine`(開く・書く・聞く・まとめて聞く・スナップショット)の一か所にまとめてあります。
hosigraph がエンジンの中身を知っているのは、ここだけです。ここを変えると、hosigraph のテスト
(Takka を基準にして、Datalevin と答えを突き合わせています)に響くことがあります。

以前は、この口が `native-bridge` という別のパッケージでした。いまは Takka の中に畳んであります。

## ディレクトリ

```
src/Takka/*.hs     ライブラリ        app/Main.hs      デモ
test/Spec.hs       テスト(76)       bench/           ベンチ・ファズ
cbits/dlmdb/       同梱した dlmdb    docs/            調査メモ・再現コード
scripts/           ghc だけでビルド
```
