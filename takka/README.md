# Takka

[hosigraph](../README.md) と [Datalevin](https://github.com/datalevin/datalevin) が、おたがいに話すときの**ことば**です。
Haskell だけで書いてあって、C も、データベースも、入っていません。

名前の takka は、パーリ語で「推論・論理」(サンスクリットの tarka と同じ根)。Datalog の logic から取りました。
もとの名前は Hevin でした。

| モジュール | なか |
|---|---|
| `Takka.Types` | 値(`Value`)、キーワード、スキーマ(`Schema`)、datom |
| `Takka.Edn` | EDN の読み書き |
| `Takka.Syntax` | Datalog のクエリと pull パターンの形(`Query`, `Input`, `Cell`, `Pulled`, …)と、EDN からの読みとり |

## むかし、ここに居たもの

はじめ、takka は dlmdb(Datalevin の LMDB フォーク)の上に Haskell で組んだ小さな Datalog データベースでした。
hosigraph の khana を、プロセスの中に持つために使っていました。
いまは外して、Datalevin だけにしています。コードとテスト(76 個)は git の履歴に残っています(commit `2aee1d7`)。
