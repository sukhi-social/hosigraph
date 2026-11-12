# Haskell ライブラリ調査メモ

実装の前に「Haskell の世界に使える仲間がいるか」を探したときのメモです。
**検索で見つけたページのタイトルや概要から判断した内容**が中心で、各パッケージの最終更新日や
細かい API までは確認していません。採用するときは Hackage のページで最新の状態を見てください。

## 見つけたもの

| 分野 | パッケージ | メモ |
|---|---|---|
| LMDB | [`lmdb`](https://hackage.haskell.org/package/lmdb) | "Lightning MDB bindings"。本家 LMDB の低レベルバインディング |
| LMDB | [`lmdb-high-level`](https://hackage.haskell.org/package/lmdb-high-level) | "Higher level API for working with LMDB" |
| LMDB | [`lmdb-bindings`](https://hackage.haskell.org/package/lmdb-bindings) | 別系統のバインディング |
| Datalog | [`souffle-haskell`](https://hackage.haskell.org/package/souffle-haskell) | Soufflé(外部の Datalog コンパイラ)を呼ぶバインディング。組み込み DB には向かない |
| Datalog | [MarkMcCaskey/datalog](https://github.com/MarkMcCaskey/datalog) | 純 Haskell の Datalog ライブラリ。永続化はない |
| RDF | [`rdf4h`](https://hackage.haskell.org/package/rdf4h) | RDF の読み書き・クエリ用ライブラリ。JSON-LD を「DB として」持つ用途とは別 |
| JSON | [`aeson`](https://hackage.haskell.org/package/aeson) | JSON-LD のパースと出力に使用 |

## 判断

* **LMDB 系は使わず、dlmdb の C ソースを同梱して直接 FFI しました。**
  Datalevin の `dlmdb` には、数え上げ・順位(`mdb_count_range`、`mdb_get_rank` など)や
  プレフィックス圧縮といった独自の拡張があります。これらは本家 LMDB 向けの既存バインディングには
  無いはず(未確認の推測です)で、クエリプランナーの見積もりに使いたかったのがいちばんの理由です。
  Haskell の FFI は小さな C ライブラリと相性が良く、`cabal` が `c-sources` として一緒に
  コンパイルしてくれるので、システムに LMDB を入れる必要もありません。
* **Datalog エンジンは自作しました。** 既存の純 Haskell 実装を DB の上に載せるより、
  「索引の件数から結合順を決める」ところを自分で持ちたかったからです。
  (`datalog` パッケージの実装を読んで比較する、というのはまだやっていません。)
* **RDF ライブラリは使わず、JSON-LD の実用的な部分集合だけ自前で。**
  EAV は最初からグラフなので、ノード = エンティティ、プロパティ = 属性という素直な写像で足ります。

## 実際に使っているパッケージ

`base`, `bytestring`, `containers`(`Data.Graph` の強連結成分も), `text`, `directory`,
`aeson`, `scientific`, `vector`(最後の 3 つは JSON-LD のため)。
テストに `QuickCheck`, `filepath`, `unix`。すべて GHC 9.4 と一緒に配られる/ディストリビューションに
入っているものだけで `cabal build` が通ります(外部ネットワーク不要)。

## 次に検討するとよさそうなもの

* **Elixir との橋渡し**: 標準入出力で EDN か JSON の 1 行 1 リクエストを受ける小さなサーバー
  (Elixir 側は `Port`)。Haskell から共有ライブラリを作って NIF にする道もありますが、
  RTS の起動と BEAM のスケジューラの付き合い方が難しいので、まず Port が無難だと思います。
* **`megaparsec`**: EDN リーダーを手書きにしたのは依存を増やしたくなかったからです。
  エラーメッセージの質を上げたくなったら置き換え候補です。
* **`unordered-containers` / `hashable`**: 大きな結果集合の結合を速くしたくなったら。
