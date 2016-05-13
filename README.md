MerkleDAG Finance Application
=============================

This repo implements a double-entry accounting system on top of a
[merkle-dag](https://github.com/greglook/clj-merkledag) data structure. The data
is loaded into an in-memory [datascript](https://github.com/tonsky/datascript)
database for indexing and querying.

It also includes a [grammar definition](resources/grammar/ledger.bnf) and parser
for [Ledger](http://ledger-cli.org/) files.

## Financial Books

This system organizes financial data into a set of independent _books_, each of
which has a balanced tree of accounts which obey the accounting equation. The
system also contains a shared set of commodity definitions and price history.

In addition to accounts each book has a _journal_, which is a sequence of
entries. Journal entry types include `open-account`, `close-account`, `notes`,
and the main entry type `transaction` and `posting`.

## Commodities and Prices

A _commodity_ is unit of value exchange, identified by a symbolic code.
Currencies are a kind of commodity, such as `USD`, `JPY`, and `CNY`. Similarly,
each stock is its own commodity, as are shares of ETFs and mutual funds.
Commodities can also be used to represent more esoteric kinds of value, like
airline miles or hours worked.

Over time, a commodity's value may change relative to other commodities. In
particular, currencies fluctuate and the values of investments grow and shrink.
The conversion rate between two commodities determines the primary commodity's
_price_ in the second (or 'base') commodity.

## Accounts

Accounts are named containers for commodities in a set of books. Each account is
identified by a colon-separated path, such as `Assets:Cash:Apple Bank:Savings`.
This forms a hierarchical tree of accounts, with 'real' accounts at the leaves.
The top-level accounts should generally be the standard 'Assets', 'Liabilities',
'Income', 'Expenses', and 'Equity'.

An account's _register_ is the history of entries applied to it. The register is
ordered by time, and the first event **must** be an `open-account` marker. If
the account has been closed, then the _last_ event must be a `close-account`
marker. In between can be notes, balance checks, and postings.

## Transactions and Postings

A _transaction_ ties together multiple _postings_ into a balanced unit. The
amounts within a transaction MUST sum to zero - this is the fundamental
principle of double-entry accounting! A posting represents a change in value
for a certain commodity within an account.

A simple transaction could contain two postings, one with an amount of
`#finance/$ [5.00M USD]` to the account `Expenses:Food:Groceries` and another
with the inverse amount `#finance/$ [-5.00M USD]` posted to
`Assets:Cash:Wallet`. The transaction could have a title like `"Grocery Store"`.
