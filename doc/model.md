Data Model
==========

This document describes the entities used to model financial data and how they
are related.


## Commodities

A commodity generically represents a type of unit of value storage and
exchange. This covers a range of different cases like:
- Government issued currencies (`USD`, `CAD`, `EUR`)
- Publicly-traded stocks (`AMZN`, `GOOG`, `BRKB`)
- Exchange-traded funds (`VTI`, `SCHE`)
- Mutual funds (`SWTSX`, `VFIAX`)
- Cryptocurrencies (`BTC`, `ETH`)
- Airline rewards miles (`points`)

Anything that can be stored and traded in some way can be represented by a
commodity, which is uniquely identified by a symbolic code.

### Quantities

Combining a numeric value with a commodity creates a _quantity_, a unit-aware
amount of value. Quantities are used all over the data model to track what
commodities are being acted upon.

### Prices

A _price_ sets a comparison point for the value of two commodities at a certain
point in time. Prices contain three pieces of information - the time, the
primary commodity being measured, and the value of that commodity as a quantity
in the base commodity being measured in.

For example, a price for commodity `X` at time `T` for `10.0 Y` says you could
exchange one `X` for ten `Y` at that time, meaning that they were worth the
same amount.

Price points are organized into a _price history_ for each commodity, ordered
by time to make time-based lookups efficient.


## Books

A book defines a full and internally consistent transaction history for a set
of accounts. Individual books should stand on their own as a source of
financial data, though they may be linked by some cross-book transfers. For
example, each book is expected to satisfy the accounting equation.

Books are also conceptually permissions boundaries, though that's not modeled
in the library.

Books contain:
- local commodities
- local price history
- accounts
- transaction journal


## Accounts

Accounts are named containers for value in a set of commodities. These
containers are structured into a hierarchy, forming a tree of accounts. The
top-level accounts should be things like 'Assets', 'Liabilities', 'Income',
'Expenses', etc.

Most accounts will probably only contain a single commodity like the local
currency, but others such as investment brokerage accounts may contain a wide
variety of commodities.


## Register Entries

An entry is an event which happens to a financial account. An account
conceptually has a _register_ of all such events, listed in the order they
occur. Each entry occurs at a certain point in time, may have a description,
reference the data source it was pulled from, etc.

### Metadata Entries

Three basic types of entries are for record keeping about the account:

The `note` entry provides a generic way to leave text notes about the account
in the history.

The `open-account` entry indicates that the account has been activated and is a
valid target for postings. The first non-note entry in a register following the
beginning or a `close-account` entry must be an `open-account` entry.

The `close-account` entry marks the account as inactive and it should not be
used in postings. There should not be any non-note entries in an account's
register until after an `open-account` entry.

### Balance Checks

Balance checks provide a mechanism for asserting the state of an account at
specific points in time. This is a tool for tying the data to known-good
numbers verified in the real world.

Each check entry specifies a specific quantity of a commodity; if the account
contains the same amount of that commodity, the check passes. If not, the check
is in error; this likely indicates missing or incorrect data somewhere.

### Postings

A posting is an entry which _changes_ the amount of some commodity in the
account. Postings generically cover the traditional "credits" and "debits" in
accounting with a single signed quantity.


## Items & Invoices

- invoices usually map to something like a receipt for a purchase
- separate top-level entity from transactions
    - what implications does this have on how they are rendered textually?
- items may belong-to an entry

...


## Transactions

...


## Journal

...


## Budgets

...
