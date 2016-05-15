(ns merkledag.data.finance.schema
  "Schema definitions for a full financial system structure.

  ```
  /<ref-name>/ -> FinanceRoot
    inputs/
      new/ -> DataQueue
      review/ -> DataQueue
      ...
    commodities/ -> CommodityData
      <commodity-symbol> -> CommodityDefinition
      ...
    prices/ -> PriceData
      <commodity-symbol>/
        <year> -> PriceHistory
        ...
      ...
    books/
      <book-name>/ -> Books
        accounts/
          assets/... -> AccountDefinition
          liabilities/...
          income/...
          expenses/...
          ...
        ledger/ -> LedgerData
          <year>/<month>/<day>/ -> LedgerHistory
            tx-01/ -> Transaction
              posting-01/ -> Posting
                invoice/ -> Invoice
                  item-01 -> LineItem
                  item-02
                  item-03
                  ...
              posting-02/...
              ...
            tx-02/...
            ...
          <year>/...
      <book-name>/
        accounts/...
        ledger/...
      ...
  ```

  Some example paths:

  ```
  /commodities/USD
  /prices/AMZN/2014
  /books/mine/accounts/assets/cash/bank/savings
  /books/house/accounts/income/rent/monthly
  /books/joint/ledger/2011/03/17/tx-04
  /books/joint/ledger/2013/10/01/tx-01/posting-02/invoice/image
  ```
  "
  (:require
    [merkledag.data.finance.types :as types]
    [merkledag.link :as link]
    [schema.core :as s :refer [defschema]])
  (:import
    java.util.UUID
    merkledag.data.finance.types.Quantity
    merkledag.link.MerkleLink
    (org.joda.time
      DateTime
      LocalDate)))


;; ## General Data Attributes

(def general-attrs
  "Definitions for generally-useful attributes."
  {:title
   {:db/doc "title to give the data value"
    :schema s/Str}

   :description
   {:db/doc "human-readable description string"
    :schema s/Str}

   :data/ident
   {:db/doc "unique identifier for data entities"
    :db/unique :db.unique/value
    :schema s/Str}

   :data/type
   {:db/doc "keyword identifying the primary entity type"
    :db/index true
    :schema s/Keyword}

   :data/tags
   {:db/doc "map of keyword tags to values"
    :schema {s/Keyword s/Any}}

   :data/sources
   {:db/doc "set of links to source documents the entity is constructed from"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :schema #{s/Any}}})


(def time-attrs
  "Definitions for time-related attributes."
   {:time/at
    {:db/doc "point in time at which the datum occurred"
     :schema DateTime}

    :time/interval
    {:db/doc "interval in time the data occurred over"
     :schema org.joda.time.Interval}})


#_
(def csv-attrs
  "Definitions for comma-separated value attributes."
  {:csv/line
   {:db/doc "original line of data parsed into the CSV"
    :schema s/Str}

   :csv/headers
   {:db/doc "original header line string"
    :schema s/Str}

   :csv/source-tag
   {:db/doc "keyword tag for the source of the data"
    :schema s/Keyword}

   :csv/data
   {:db/doc "map of parsed header/data pairs"
    :schema {s/Keyword s/Str}}})



;; ## Helper Functions

(defn- constrained-keyword
  "Creates a schema for a keyword with a mandatory namespace component. If
  names are provided, the schema is an enumeration."
  ([ns]
   (s/constrained s/Keyword #(= ns (namespace %))))
  ([ns names]
   (apply s/enum (map #(keyword ns (name %)) names))))


(defn- distribution-map
  "Creates a schema for a key type which can either be a single keyword or a map
  of keys whose values sum to one."
  [key-schema]
  (s/conditional map? (s/constrained
                        {key-schema s/Num}
                        #(== 1 (reduce + (vals %))))
                 :else key-schema))


(defn- build-schema
  "Helper function to merge many attribute map definitions. The first argument
  is a keyword which the `:data/type` value is required to equal. Remaining
  arguments should be key/value pairs mapping attribute map vars to a collection
  of keywords naming the required attributes. The remaining attributes are
  considered optional."
  [data-type & {:as attr-specs}]
  (->
    attr-specs
    (->>
      (merge {general-attrs [:data/type]})
      (mapcat
        (fn spec->schemas
          [[attrs required-keys]]
          (let [optional? (complement (set required-keys))]
            (map (fn schema-key
                   [[attr-key schema-def]]
                   [(cond-> attr-key
                      (optional? attr-key) s/optional-key)
                    (:schema schema-def)])
                 attrs))))
      (into {}))
    (dissoc (s/optional-key :data/type))
    (assoc :data/type (s/eq data-type))))



;; ## Asset Classes

(def asset-class-names
  "Set of names for some common asset classes."
  #{:cash
    :intl-government-bond
    :intl-corporate-bond
    :us-government-bond
    :us-corporate-bond
    :us-municipal-bond
    :intl-developed-stock
    :intl-emerging-stock
    :us-large-cap-value
    :us-large-cap-core
    :us-large-cap-growth
    :us-mid-cap-value
    :us-mid-cap-core
    :us-mid-cap-growth
    :us-small-cap-value
    :us-small-cap-core
    :us-small-cap-growth
    :currency
    :real-estate
    :gold
    :commodities
    :other})


(defschema AssetClassKey
  "Schema for a keyword identifying an asset class."
  (constrained-keyword "finance.commodity.class" asset-class-names))


(defschema AssetClassBreakdown
  "Schema for a map of asset classes to proportional numbers. The values in the
  map must sum to 1."
  (distribution-map AssetClassKey))



;; ## Commodity Sectors

(def commodity-sector-names
  "Set of names for some common commodity sectors."
  #{:basic-materials
    :communication-services
    :consumer-cyclical
    :consumer-defensive
    :energy
    :financial-services
    :healthcare
    :industrials
    :technology
    :utilities})


(defschema CommoditySectorKey
  "Schema for a keyword identifying a commodity sector."
  (constrained-keyword "finance.commodity.sector" commodity-sector-names))


(defschema CommoditySectorBreakdown
  "Schema for a map of sectors to proportional numbers. The values in the map
  must sum to 1."
  (distribution-map CommoditySectorKey))



;; ## Commodities

(defschema CommodityCode
  "Schema for a symbol identifying a commodity."
  (s/constrained s/Symbol #(re-matches #"[a-zA-Z][a-zA-Z0-9_]*" (str %))))


(def commodity-attrs
  "Attribute schemas for commodities."
  {:finance.commodity/code
   {:db/doc "code symbol used to identify the commodity"
    :db/unique :db.unique/identity
    :schema CommodityCode}

   :finance.commodity/currency-symbol
   {:db/doc "one-character string to prefix currency amounts with"
    :schema (s/constrained s/Str #(= 1 (count %)))}

   :finance.commodity/asset-class
   {:db/doc "map of asset class breakdowns or single class keyword"
    :schema AssetClassBreakdown}

   :finance.commodity/commodity-sector
   {:db/doc "map of asset class breakdowns or single class keyword"
    :schema CommoditySectorBreakdown}})


(defschema CommodityDefinition
  "Schema for a commodity definition directive."
  (build-schema :finance/commodity
    commodity-attrs [:finance.comomdity/code]))



;; ## Prices

(def price-attrs
  "Datascript attribute schemas for commodity price points."
  {:finance.price/commodity
   {:db/doc "the commodity the price is measuring"
    :db/valueType :db.type/ref
    :schema CommodityCode}

   :finance.price/value
   {:db/doc "amount of the base commodity a unit of this commodity costs"
    :schema Quantity}})


(defschema CommodityPrice
  "Schema for an explicit price point."
  (build-schema :finance/price
    price-attrs [:finance.price/commodity
                 :finance.price/value]
    time-attrs [:time/at]))



;; ## Items and Invoices

;; A line item represents a transacted amount of a product at a certain price.
;; A common example is a line on a receipt, showing the purchase of an item.
;;
;; Typically, the _amount_ will be a bare number or a physical quantity. The
;; _price_ is the unit price of the item, as a financial quantity. The item
;; _total_ multiplies the two and takes the commodity specified in the price.
;; This is rendered like:
;;
;;     $20.18 (2.0 lb @ $10.09)
;;
;; When the amount is a financial quantity and the price is a regular number,
;; it's considered to be a percentage of the amount. One common example is tax,
;; which is applied as a fraction of the bill total. This is rendered like:
;;
;;     $12.22 ($127.29 @ 9.6%)

(def item-attrs
  "Attribute schemas for account properties."
  {:finance.item/rank
   {:db/doc "number giving the order the item appears on its invoice"
    :schema s/Num}

   :finance.item/total
   {:db/doc "total amount contributed by this item"
    :schema Quantity}

   :finance.item/amount
   {:db/doc "Amount of the item on the invoice. A bare number indicates a
            unitless amount of items transacted."
    :schema (s/conditional number? s/Num :else Quantity)}

   :finance.item/price
   {:db/doc "Price per unit of the item. A bare number is treated as a unit
            percentage multiplier."
    :schema (s/conditional number? s/Num :else Quantity)}

   :finance.item/vendor
   {:db/doc "Additional string describing the vendor the item is from."
    :schema s/Str}

   :finance.item/tax-groups
   {:db/doc "Set of keywords indicating the tax groups a given item is part of."
    :schema #{s/Keyword}}

   :finance.item/tax-applied
   {:db/doc "Keyword indicating the group this tax item applies to."
    :schema s/Keyword}})


(defschema LineItem
  "Schema for a line-item in an invoice."
  ; TODO: validations
  ; - amount and price only make sense if total is set
  ; - amount and price must be set together
  ; - total should equal amount * price (or be within tolerance)
  (build-schema :finance/item
    general-attrs [:title]
    item-attrs []))


(def invoice-attrs
  "Attribute schemas for invoices."
  {:finance.invoice/items
   {:db/doc "Collection of items that make up the invoice."
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :schema [LineItem]}})


(defschema Invoice
  "Schema for a collection of line items in an invoice."
  (build-schema :finance/invoice
    invoice-attrs [:finance.invoice/items]))



;; ## Accounts

(def account-type-names
  "Set of names for some common account types."
  #{:cash
    :checking
    :savings
    :certificate-of-deposit
    :brokerage
    :traditional-401k
    :roth-401k
    :traditional-ira
    :roth-ira
    :bitcoin})


(defschema AccountTypeKey
  "Schema for a keyword identifying an account type."
  (constrained-keyword "finance.account.type" account-type-names))


(defschema AccountPath
  [(s/one s/Str "root") s/Str])


(defschema AccountAlias
  s/Keyword)


(defschema AccountRef
  "Schema for a reference to an account by path or by alias."
  (s/conditional vector? AccountPath
                 :else AccountAlias))


(def account-attrs
  "Attribute schemas for account properties."
  {:finance.account/book
   {:db/doc "name of the book the account is part of"
    :schema s/Str}

   :finance.account/id
   {:db/doc "link to the account's root node"
    :db/unique :db.unique/identity
    :schema s/Str}

   :finance.account/path
   {:db/doc "path segments to uniquely identify the account"
    :db/unique :db.unique/identity
    :schema AccountPath}

   :finance.account/alias
   {:db/doc "keyword alias to refer to the account by"
    :db/index true
    :schema AccountAlias}

   :finance.account/type
   {:db/doc "keyword identifying the type of account"
    :schema AccountTypeKey}

   :finance.account/external-id
   {:db/doc "string giving the account's external identifier, such as an account number"
    :db/unique :db.unique/identity
    :schema s/Str}

   :finance.account/allowed-commodities
   {:db/doc "set of commodities which are valid for the account to contain"
    :db/cardinality :db.cardinality/many
    :schema #{CommodityCode}}})


(defschema AccountDefinition
  "Schema for an object defining the properties of an account."
  (build-schema :finance/account
    account-attrs [:finance.account/book
                   :finance.account/path]))



;; ## Journal Entries

(def entry-attrs
  "Datascript attribute schemas for journal entries."
  {:finance.entry/account
   {:db/doc "name of the account the entry is related to"
    :db/valueType :db.type/ref
    :schema AccountRef}

   :finance.entry/rank
   {:db/doc "extra numeric value to determine the ordering of entries with the same timestamp"
    :schema s/Num}})


(def balance-attrs
  "Attribute schemas for balance check entries."
  {:finance.balance/amount
   {:db/doc "amount of a certain commodity the account should contain"
    :schema Quantity}})


(defschema AccountNote
  "General annotation and document linking to accounts."
  (build-schema :finance.entry/note
    time-attrs [:time/at]
    entry-attrs [:finance.entry/account]))


(defschema OpenAccount
  "Opening marker for an account."
  (build-schema :finance.entry/open-account
    time-attrs [:time/at]
    entry-attrs [:finance.entry/account]))


(defschema CloseAccount
  "Tombstone marker for an account."
  (build-schema :finance.entry/close-account
    time-attrs [:time/at]
    entry-attrs [:finance.entry/account]))


(defschema BalanceCheck
  "Assertion that an account contains a specific amount of a commodity."
  (build-schema :finance.entry/balance-check
    time-attrs [:time/at]
    entry-attrs [:finance.entry/account]
    balance-attrs [:finance.balance/amount]))



;; ## Postings

(def posting-attrs
  "Attribute schemas for posting entries."
  {:finance.posting/virtual
   {:db/doc "boolean flag indicating that the posting is virtual and need not balance"
    :schema s/Bool}

   :finance.posting/payee
   {:db/doc "string name for the counterparty of this posting"
    :schema s/Str}

   :finance.posting/amount
   {:db/doc "quantity of a commodity that is changed in the account"
    :schema Quantity}

   :finance.posting/price
   {:db/doc "price per-unit of the commodity in `amount` the posting took place at"
    :schema Quantity}

   :finance.posting/weight
   {:db/doc "if `price` is set, rather than relying on multiplying the amount by
            the price, an explicit balance weight can be given"
    :schema Quantity}

   :finance.posting/lot
   {:db/doc "reference to the posting which established the position this posting is altering"
    :db/valueType :db.type/ref
    :schema MerkleLink}

   :finance.posting/lot-date
   {:db/doc "date used to establish the lot"
    :schema LocalDate}

   :finance.posting/cost
   {:db/doc "amount per unit that the commodity amount originally cost"
    :schema Quantity}

   :finance.posting/invoice
   {:db/doc "reference to an itemized list for the posting amount"
    :db/valueType :db.type/ref
    :schema Invoice}})


(defschema Posting
  "Schema for a financial posting to an account."
  ; TODO: validations
  ; - amount and price must have different commodities
  ; - weight only makes sense when price is specified
  ; - weight must be in same commodity as price
  ; - weight must be within tolerance of amount * price
  ; - balance should match commodity in amount
  ; - total of items in invoice must match amount
  ; - lot-id should specify a real previous posting
  ; - lot-cost and lot-date should match identified posting
  (build-schema :finance.entry/posting
    time-attrs [:time/at]
    entry-attrs [:finance.entry/account]
    balance-attrs []
    posting-attrs []))



;; ## Transactions

(defschema JournalEntry
  (->>
    {:note AccountNote
     :open-account OpenAccount
     :close-account CloseAccount
     :balance-check BalanceCheck
     :posting Posting}
    (mapcat (fn [[kw schema]]
              [#(= (keyword "finance.entry" (name kw)) (:data/type %)) schema]))
    (apply s/conditional)))


(def transaction-attrs
  "Datascript attribute schemas for transactions."
  {:finance.transaction/date
   {:db/doc "local calendar date on which the transaction occurred"
    :schema LocalDate}

   :finance.transaction/entries
   {:db/doc "references to child journal entries"
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :schema [JournalEntry]}

   :finance.transaction/links
   {:db/doc "string identifiers linking transactions together"
    :db/cardinality :db.cardinality/many
    :db/index true
    :schema #{s/Str}}

   :finance.transaction/flag
   {:db/doc "optional flag value to apply to postings"
    :schema s/Keyword}})


(defschema Transaction
  "Schema for an object representing a financial transaction."
  ; TODO: validations
  ; - real posting weights must sum to zero
  (build-schema :finance/transaction
    general-attrs [:title]
    transaction-attrs [:finance.transaction/date
                       :finance.transaction/entries]))



;; ## All Attributes

(def db-schema
  "A combination of every attribute map suitable for creating a datascript
  database from."
  (merge general-attrs
         time-attrs
         #_ csv-attrs
         commodity-attrs
         price-attrs
         item-attrs
         invoice-attrs
         account-attrs
         entry-attrs
         balance-attrs
         posting-attrs
         transaction-attrs))
