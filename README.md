Ledjer Finance Application
==========================

This project implements a double-entry accounting system in Clojure which uses
an in-memory [datascript](https://github.com/tonsky/datascript) database for
indexing and querying. Data can be imported from [Ledger](http://ledger-cli.org/)
files with a somewhat [expanded syntax](resources/grammar/ledger.bnf). Data is
persisted as a [merkle-dag](https://github.com/greglook/clj-merkledag) data
structure.

Ledjer organizes financial data into a set of independent _books_, each of
which has a balanced tree of accounts which obey the accounting equation. Books
allow for top-level separation, for example tracking your personal finances
versus the finances for a club or small business.

Each book has a _journal_, which is a sequence of transactions, each of which
contains one or more _entries_ such as postings. In addition, all books use a
shared set of commodity definitions and price history.

## Commodities and Prices

A _commodity_ is unit of value exchange, identified by a symbolic code.
Currencies are a kind of commodity, such as `USD`, `EUR`, and `CNY`. Similarly,
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
ordered by time, and the first entry **must** be an `open-account` marker. If
the account has been closed, then the _last_ entry must be a `close-account`
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

## Invoices and Items

A _line item_ is a detailed breakdown of part of a posting amount. This is like
a single line on a receipt from a store: the individual item purchased forms
part of the total amount which is posted to the appropriate expense account.

An item may have an _amount_, which can be a bare number or a physical quantity.
The _price_ is the unit price of the item, as either a quantity or a percentage
scalar. The item _total_ multiplies the two and takes the commodity specified in
the price. This is rendered like so:

    $20.18 (2.0 lb @ $10.09)

When the amount is a financial quantity and the price is a regular number,
it's considered to be a percentage of the amount. One common example is tax,
which is applied as a fraction of the bill total. This is rendered like:

    $12.22 ($127.29 @ 9.6%)

Invoices may be shared among multiple postings if relevant.
