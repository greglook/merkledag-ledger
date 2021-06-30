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
- set of accounts
- journal of transactions
- list of invoices
- budget history


## Accounts

Accounts are named containers for value in a set of commodities. These
containers are structured into a hierarchy, forming a tree of accounts. The
top-level accounts should be things like 'Assets', 'Liabilities', 'Income',
'Expenses', etc.

Most accounts will probably only contain a single commodity like the local
currency, but others such as investment brokerage accounts may contain a wide
variety of commodities.


## Register Entries

An entry is an event which happens to a financial account. An account has a
_register_ of all such events, listed in the order they occur. Each entry
occurs at a certain point in time, may have a description, reference the data
source it was pulled from, etc.

### Metadata Entries

Three basic types of entries are for record keeping about the account:

The `note` entry provides a generic way to leave text notes about the account
in the history.

The `open-account` entry indicates that the account has been activated and is a
valid target for postings. The first non-note entry in a register following the
beginning or a `close-account` entry must be an `open-account` entry.

The `close-account` entry marks the account as inactive and it should not be
used in postings. There should not be any non-note entries in an account's
register until after another `open-account` entry.

### Balance Checks

Balance checks provide a mechanism for asserting the state of an account at
specific points in time. This is a tool for tying the data to known-good
numbers verified in the real world.

Each check entry specifies a specific quantity of a commodity; if the account
contains the same amount of that commodity, the check passes. If not, the check
is in error; this likely indicates missing or incorrect data somewhere.

**TODO:** are these evaluated in _transaction order_ or _register order_?

### Postings

A posting is an entry which _changes_ the amount of some commodity in the
account. Postings generically cover the traditional "credits" and "debits" in
accounting with a single signed quantity.


## Transactions

The primary historical entity in the data model is the _transaction_, which
contains one or more entries. Transactions occur on a date (but not specific
times!) and may be ordered relative to each other within that day. They
typically contain information like the title, a description, and other metadata
tags that help organize them.

The entries in a transaction may (and usually do) affect multiple accounts, and
critically **must balance** to preserve the double-entry accounting invariant.
This means that the sum of the weight of all entries must equal zero.

### Journal

The journal is the historical list of transactions in the book.


## Invoices

An invoice is a document which gives a per-item breakdown of some exchange,
usually of a currency for a set of goods. Some typical examples of invoices
are:
- a receipt you get at the store after buying some items
- a quote for some work done on a project
- a confirmation email after placing an online order

Invoices are separate top-level entities from transactions; typically an
invoice will correspond to one or more postings against some accounts, but it's
not necessarily 1:1.

### Line Items

Invoices contain _line items_, which are individual charges that roll up to the
total amount in the invoice. Items have their own fields for units, unit cost,
line amount, etc. Each item may link to a posting that it contributes its
amount to.

For example, a receipt from a grocery store might have several items that
contribute to a single posting to `Expenses:Food:Groceries`, while other items
are part of `Expenses:Entertainment:Alcohol`, `Expenses:Home:Supplies`, etc.


## Budgets

Budgets represent pools of money for projecting and managing flows of money
through the accounts in a book. Each budget represents a pool of money the
book has which is allocated for a specific purpose; examples might be "Food",
"Emergencies", "Utilities", etc. Each budget has a name, a target daily rate,
and a maximum amount. Budgets are organized into a priority list, with earlier
budgets being considered "more important".

By default, there is an "unbudgeted" pool that contains all of the value that
has not been allocated to a specific budget. This pool is also drawn from when
expenses are not applied against a specific budget.

### Budgeted Accounts

In addition to defining the budget pools themselves, the user must also declare
which accounts participate in budgeting. Individual dollars or budgets are not
assigned to specific accounts, but conceptually the total budget pool balance
should equal the balance of all the budgeted accounts.

Whenever a transaction changes the balance of a budgeted account, the other
postings against non-budgeted accounts in the transaction represent "flows" to
or from the budgets. A positive amount on a flow posting represents a budget
outflow like an expense, and should be tagged with the budget it is spending
against. A negative amount represents a budget inflow like income, and is put
into the unbudgeted pool by default.

Commonly, accounts like checking, savings, and credit cards will be budgeted.
For something longer term and more nebulous like investment accounts, it
probably makes more sense to have an "investments" budget but _not_ tag the
investment account itself. Then the budget represents how much money can be
transferred to be invested; the positive change to the investment account is an
outflow towards the budget goal.

### Budget Allocation

At the start of each day, the following allocation algorithm is run. For each
budget, in order of the user's priority list:

1. Compare the budget's current balance to the maximum. If the limit has been
   reached, move to the next budget.
2. Take the minimum of the target daily rate, the distance to the balance cap,
   and the available unallocated pool. Allocate this to the current budget.
3. Reduce the unallocated pool by the amount from the previous step. If any
   value is left, move to the next budget.
4. If there are no more budgets, leave the remaining amount in the unallocated
   pool.

At any time the user may manually reallocate value between the budgets and
unallocated pool.
