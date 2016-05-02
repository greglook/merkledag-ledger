MerkleDAG Finance Application
=============================

This repo contains code implementing a double-entry accounting system on top of
a [merkle-dag](https://github.com/greglook/clj-merkledag) data structure. It
includes a [grammar definition](resources/grammar/ledger.bnf) and parser for
[Ledger](http://ledger-cli.org/) files.

## Ideas

- Store a graph of merkle nodes in a merkledag repo
- Every node should declare a `:data/ident` string giving a random unique
  identifier.
- Client subscribes to new blocks over websocket?
- When updates happen, send to client side and apply updates to datascript
  database.
- Use datascript db for indexing and queries.
