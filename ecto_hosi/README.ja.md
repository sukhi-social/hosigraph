# ecto_hosi

[hosigraph](https://github.com/sukhi-social/hosigraph)(出来事を足していくだけのログが真実で、「今」は Datalog DB に畳み込んだコピー)のための Ecto アダプター。
Ecto の `Repo` / `Changeset` / `preload` はそのまま使えて、Datalog ならではの**ルール(再帰を含む)**も書けます。

> 状態: 0.1。本物の hosigraph + Datalevin 1.2.10 で、テスト 52 本が通っています(下の「確かめたこと・確かめていないこと」)。
> ルールを渡す口(rules input)は、はじめ hosigraph 0.2 に無く、このアダプター用のパッチで足していました。いまは hosigraph 本体に入っています(PR #27)。
> 仕様と、試して分かったことは [docs/rules-input-contract.md](docs/rules-input-contract.md)。

## 使い方

```elixir
defmodule MyApp.Repo do
  use Ecto.Repo, otp_app: :my_app, adapter: Ecto.Adapters.Hosi
end

config :my_app, MyApp.Repo,
  dir: "/var/lib/my_app/hosi",
  schemas: [MyApp.User, MyApp.Post, MyApp.Follow]

defmodule MyApp.User do
  use Ecto.Schema
  use Ecto.Hosi.Schema          # 任意。hosi_unique を使うときだけ
  hosi_unique :email            # hosigraph 自身が同じ email を二度と受けつけない

  schema "user" do
    field :name, :string
    field :email, :string
    has_many :posts, MyApp.Post
    timestamps(type: :utc_datetime_usec)
  end
end
```

行は「事実」になります。`user` の `name` は `[id :"user/name" "Ada"]`。主キー `:id` が hosigraph のエンティティ ID です。
hosigraph にテーブルはないので、各行に目印の属性 `:"user/ecto-kind"` も付きます(「すべてのユーザー」を探すための印)。

## Ecto のまま使えるもの

`insert` `update`(`optimistic_lock` つき) `delete` `insert_all` `get` `get_by` `all` `one` `exists?` `aggregate(:count)` `preload` と、changeset の `unique_constraint`。

クエリは1つのスキーマに対して、`where`(`== != < <= > >= in is_nil not and or`)、`order_by`、`limit`、`offset`、フィールドの `select`。
それ以上(join など)は、わざと例外にして `Repo.datalog/1` を案内します。

## Ecto と違うところ

* **トランザクションはありません。** hosigraph の書き込みは「1つの束を丸ごと」なので、Ecto の1歩ずつのトランザクションとは合いません。
  代わりに `Repo.write_batch/2`(下)で、複数行を全部成功か全部失敗で書けます。
  関連つきの `Repo.insert` は、親と子を順に書きます(まとめては失敗しません)。
* **`not` は「そういう事実がない」です。** `where: not (u.age > 30)` は、年齢の事実がない行も拾います(SQL は `NULL` の行を拾いません)。`!=` `<` などは、事実があることが必要で、SQL と同じです。
* `update_all` / `delete_all` / `stream` はありません。マイグレーションもありません(hosigraph のスキーマは Ecto のスキーマから、Repo の起動時に作ります)。

## Repo に足される関数

```elixir
# ルール(再帰つき)で読む
defmodule MyApp.Rules do
  use Ecto.Hosi.Rules

  # a は b にたどり着ける = a が b をフォローしている、または a のフォロー先からたどり着ける
  rule :reachable, [a, b] do
    match MyApp.Follow, from: a, to: b
  end

  rule :reachable, [a, c] do
    match MyApp.Follow, from: a, to: b
    reachable(b, c)
  end
end

import Ecto.Hosi.Query
me = 1
MyApp.Repo.datalog(datalog find: [b], where: [reachable(^me, b)], rules: MyApp.Rules)
#=> [[2], [3], [4]]

Repo.history(id)                      # そのエンティティの全イベント
Repo.highest(Post, :inserted_at, 20)  # 新しい順のページ。{structs, last}、次は after: last
Repo.erase([[:ids, [id]]])            # 本当に消す(退会など)

Repo.write_batch(                     # 全部成功か全部失敗の1回の書き込み
  insert: {:user, user_changeset},
  insert: {:post, post_changeset, refs: [user_id: :user]},   # まだ存在しない親を指せる
  update: {:other, other_changeset},
  delete: {:old, old_struct}
)
#=> {:ok, %{user: %User{}, post: %Post{}, ...}}   または {:error, 名前, changesetか :stale}
```

ルールは**コンパイル時に**次を確かめます: 頭・比較・`not` の変数がどこかで束縛されていること、同じルールの引数の数、フィールド名がスキーマにあること、
そして**層化**(`not` を通して自分自身に戻るルールがないこと。そういうルールには答えが1つに決まりません)。

## hosigraph が Datalevin を使うための設定

```elixir
config :my_app, MyApp.Repo,
  dir: "/var/lib/my_app/hosi",
  schemas: [...],
  open: %{config: %{khana: [:dtlv, "/opt/datalevin/dtlv"]}}
  # JVM 常駐なら: open: %{config: %{khana: [:jvm, "java", "/opt/datalevin/datalevin-1.2.10-standalone.jar"]}}
```

Repo の起動時に、アダプターが VM の `SIGCHLD` を既定に戻します(VM 全体の設定です)。BEAM は `SIGCHLD` を無視して起動するので、
中の Haskell が `dtlv` / `java` の終了を待てず `waitForProcess: does not exist (No child processes)` になるためです。

## 設定

| キー | 意味 |
|---|---|
| `:dir` | hosigraph のディレクトリ(`mix ecto.create` / `ecto.drop` でも使う) |
| `:schemas` | hosigraph に置く Ecto スキーマ |
| `:open` | hosigraph を開くときの追加オプション |
| `:precheck_unique` | 既定 `true`。`hosi_unique` の値が使われていないか、書く前に問い合わせる |
| `:hosi` | hosigraph と話すモジュール(既定 `Hosi`)。アプリのテストでは `hosi: Ecto.Hosi.Fake` にすると、libhosi なしでメモリ上で動く |

## テスト

```sh
HOSI_BUILD=none mix test      # libhosi なしで動く(メモリ上の替え玉 Ecto.Hosi.Fake を使う。Fake はこのパッケージに入っています)

# 本物の hosigraph + Datalevin に向ける(上のルールの口のパッチを当てた hosigraph で)
HOSI_PATH=/path/to/hosigraph/hosigraph-ex HOSI_DTLV=/path/to/dtlv HOSI_REAL=1 mix test
```

53 テスト。替え玉は「hosigraph の約束どおり」に作ってあります(`temps`、`when:`、`[:ref, id]`、型チェック、`%` なしのルール呼び出しの拒否、Datalevin の unique のエラー文)。
`HOSI_REAL=1` では、各テストが新しいディレクトリで本物を開きます(1本あたり約0.5秒。替え玉の中身を見る1本だけ除きます)。
**替え玉が通ることと、本物が通ることは別でした:** 替え玉は、頭の変数名が節ごとに違う再帰ルールも、順番つきの答えも、通してしまっていました。本物で試して直したものです。

## 確かめたこと・確かめていないこと

確かめた(動かした):
* **本物の hosigraph(ソースからビルド)+ Datalevin 1.2.10(ネイティブ `dtlv`)で、このアダプターのテスト 52 本が通る**(失敗 0)。
  `insert` `update`(楽観ロック) `delete` `insert_all` `preload` `unique_constraint`、`where` の各種、`history` `highest` `erase`、`write_batch` の原子性、
  そして**再帰ルール(循環つき)・`not` つきルール・比較つきルール**。
* hosigraph 自身のテストが 147 本通る(ルールの口のテスト1本を足したもの)。
* Ecto 3.15.0-dev(master)でコンパイルでき、警告なし。

確かめていない:
* ルールの口は、**hosigraph に私が足したパッチ**の上でだけ動いています。hosigraph 本体にはまだありません。
* ビルドは **GHC 9.4.7**(aptのもの)で行いました。README の GHC 9.10.3 では動かしていません(そちらには、互換パッチは不要なはずです)。
* Datalevin の JVM 常駐モードでの、このアダプターのテスト。(hosigraph 自身のテストは、JVM で 20 シナリオが通りましたが、時間切れで全部は回っていません。)
* macOS、Ecto 3.12〜3.14。
* 大きなデータ。テストは数行〜数十行の世界です。`order_by` / `limit` は、一致した行を全部読んでから Elixir 側で行います(`highest` が近道です)。
