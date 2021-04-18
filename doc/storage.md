Data Storage
============

This document discusses the various ways in which a dataset can be represented,
both in memory and as a file "on disk" somewhere.

**TODO:** how can the data reference document files? maybe a generic URI and
let the hosting system handle it?


## Representations

What representations of the dataset are there?

### Entity Collection

One simple representation of the full dataset is conceptually a flat collection
of entity data. In memory, this would be a sequence of maps with attribute keys
and values.

We could answer queries over the data by iterating over the whole collection
and filtering out data we didn't need. That would obviously be inefficient, but
the goal of this representation is its simplicity.

There is intentionally no structure provided in this collection to emphasize
that each entity must contain all of the data necessary to represent it,
including some kind of type keyword. While there is no required ordering of the
entities, the _preferred_ ordering should be:
- the book entity
- commodities
- price history
- accounts, ordered depth first
- the transaction journal, in time order:
    - the transaction
    - any invoices
    - entries such as postings, balance checks, etc

### Datascript DB

Another representation we want in order to efficiently answer queries is a
Datascript database. This is also an in-memory representation and contains
several index structures with entity/attribute/value tuples.

### Entity File Tree

A simple on-disk representation of the dataset extends the idea of the entity
collection to write out each entity to a file in a directory tree structure.
The individual files could be written in EDN or CBOR.

The goal of this representation would be for ease of interop with other
systems, and to let the user see a direct view of the structured data.

### Packed Dataset

The default persisted representation will probably be an efficient on-disk
format which combines all of the data into one or more packed files. The
primary goal is efficient conversion to and from the in-memory representations.
The format should support efficient random reads and incremental writes.

Is there a way to use sqlite for this?

### Text Ledger

The dataset can be rendered (possibly "lossily") in an extended grammar of the
Ledger text format. This allows for existing `ledger` tooling to operate on the
data, as well as potentially supporting hand-editing. The data may be spread
over several files for organization, linked with `include` directives.


## Operations

Given these formats, what are the operations we need to be able to do?

- Read saved data into a datascript db for querying.
- Update packed dataset with incremental changes.
- Diff two data versions.
- Write out ledger data. Diff with existing ledger data.
- Run queries over the data to produce graphs.

The "entity collection" representation can be the common interchange among all
the other formats.
