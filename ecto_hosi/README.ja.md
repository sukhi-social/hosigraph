# ecto_hosi

[hosigraph](https://github.com/sukhi-social/hosigraph)(出来事を足していくだけのログが真実で、「今」は Datalog DB である
Datalevin に畳み込んだコピー)のための Ecto アダプターです。

Ecto の `Repo` / スキーマ / `Changeset` / `preload` はそのまま使えます。そのうえで、**テーブルではなく事実**、
**再帰を含むルールを書ける Datalog**、どのレコードの**履歴**も読めること、**本当に消せること**が手に入ります。
English: [README.md](README.md)

> Hex には出していません。試したのは Linux aarch64 だけです。この版はまだリリースしていません。0.2.0 が書いたデータは
> 読めません([変更履歴](../CHANGELOG.md))。

## 入れ方

```elixir
# mix.exs
{:ecto_hosi, github: "sukhi-social/hosigraph", sparse: "ecto_hosi"}
```

ほかに次の2つが要ります。

* **libhosi**(hosigraph の本体をコンパイルしたもの)。ビルド済みの配布は、それを公開したリリースにだけあります。
  ソースからは GHC 9.10.3 と cabal で `cabal build hosigraph-abi:flib:hosi` を実行し、アプリは
  `HOSI_BUILD=source HOSI_LIB_DIR=<libhosi.so のある場所> HOSI_INCLUDE_DIR=<hosigraph-abi/gen/include>`
  を付けてコンパイルします([hosigraph-ex](../hosigraph-ex/README.md#getting-libhosi)。`HOSI_LIB_DIR` を使う方法は、ここでは試していません)。
* **Datalevin** 1.2.10。JDK 21 以上と単体の jar、またはネイティブの `dtlv`。

## まず動かす

```elixir
defmodule MyApp.Repo do
  use Ecto.Repo, otp_app: :my_app, adapter: Ecto.Adapters.Hosi
end

config :my_app, MyApp.Repo,
  dir: "/var/lib/my_app/hosi",
  schemas: [MyApp.Note, MyApp.Pinned],
  khana: [jvm: "/opt/datalevin/datalevin-1.2.10-standalone.jar"]

# スキーマは「事実の見え方(ビュー)」です
defmodule MyApp.Note do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where type: "note"          # `type` が :note のレコードがノート
  hosi_owner :author_id            # 作った人。そのアカウントを消すときに一緒に消える
  hosi_keyword :type, ["note"]
  hosi_record_type :type

  schema "note" do                 # 名前は Ecto の都合です。属性の前には何も付きません
    field :type, :string, default: "note"
    field :author_id, :id
    field :content, :string        # 属性 `content`
  end
end

defmodule MyApp.Pinned do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where :pinned_at            # この事実を持つレコードすべて
  hosi_owner :nobody

  schema "pinned" do
    field :pinned_at, :utc_datetime_usec
    field :content, :string        # ノートの content と同じ属性
  end
end

note = MyApp.Repo.insert!(%MyApp.Note{author_id: me, content: "hello"})
#=> 事実: [id :type :note] [id :author_id me] [id :content "hello"] [id :hosi/owner me]

MyApp.Repo.update!(Ecto.Changeset.change(%MyApp.Pinned{id: note.id}, pinned_at: DateTime.utc_now()))
MyApp.Repo.get!(MyApp.Pinned, note.id).content   #=> "hello"
MyApp.Repo.explain(from n in MyApp.Note, where: n.content == "hello")
```

`mix hosi.doctor` で必要なものがそろっているか調べられます。使い方は **[ガイド(英語)](docs/guide.md)** に、
事実とビュー → Ecto から Datalog へ → 作成・更新・削除・消去 → トランザクション → 限界、の順で書いてあります。

## できること

* **Ecto:** `insert` `update`(`optimistic_lock` つき) `delete` `insert_all`(`on_conflict` つき) `update_all`
  `delete_all` `get` `get_by` `all` `one` `exists?` `aggregate` `preload`、join、`group_by`、集計、
  `unique_constraint`、`foreign_key_constraint`、`Repo.transaction` と `Ecto.Multi`。
* **ビューとルール:** 共有の語彙(`Ecto.Hosi.Vocabulary`)、複数の値、どのレコードも指せる参照、キーワード、UUID、
  unique / final(変えられない) / レコードの種類 / ファイル名のルール、`on_delete`、墓石(tombstone)。
* **Repo に足される関数:** `explain` · `datalog`(ルール、`load:`)· `highest` · `highest_in` · `facts` ·
  `history` · `write_batch` · `write_facts` · `erase` · `erase_account` · `clean`
* **Mix タスク:** `mix hosi.doctor`、`mix hosi.attributes`

## SQL と違うところ

* レコードは、持っている事実によってビューに現れます。どのビューを通した更新も、事実を変えるだけです。
* `delete` は何を消すかがはっきりしていて、ビューに見えていない事実は消しません(断ります)。
* `not` は「そういう事実がない」という意味です。
* トランザクションは SQL のものではありません。1回の束の書き込み(`write_batch`)は原子的で、それ以外はロールバックで書き戻します。
* サブクエリ、ウィンドウ関数、`union`、`lock`、`with_cte`、`stream`、名前の文字列によるクエリ、マイグレーションはありません。

## 設定

| キー | 意味 |
|---|---|
| `:dir` | hosigraph のディレクトリ |
| `:schemas` | ここに置くビュー |
| `:khana` | `[jvm: jar]`、`[jvm: jar, java: パス]`、`[dtlv: パス]` のどれか |
| `:open` | hosigraph を開くときの追加オプション |
| `:timeout` | 書き込みを待つ時間(ミリ秒)。既定 15000 |
| `:precheck_unique` | 既定 `true`。unique の値が使われていないか、書く前に問い合わせる |
| `:join` | 同じデータベースを使う2つ目の Repo |
| `:hosi` | hosigraph と話すモジュール(既定 `Hosi`) |

## アプリのテスト

`config :my_app, MyApp.Repo, hosi: Ecto.Hosi.Fake` にすると、libhosi も Datalevin もなしにメモリ上で動きます。
よく使う呼び出しについては、本物が断るものを同じように断ります。ただし替え玉なので、大事な道は本物でも確かめてください。

## 開発

```sh
HOSI_BUILD=none mix test      # 替え玉で

# 本物で(ネイティブの dtlv を使うときは HOSI_JAR の代わりに HOSI_DTLV=<dtlv>)
HOSI_REAL=1 HOSI_BUILD=source HOSI_JAR=<datalevin.jar> mix test
```

2つを切り替えたあとは `mix deps.compile hosi --force` が必要です。
本物で動かしたのは、GHC 9.10.3、JVM で動かす Datalevin 1.2.10、Ecto 3.14.3 の組み合わせです。macOS、ネイティブの `dtlv`、
Ecto 3.12 / 3.13、大きなデータでは動かしていません。
