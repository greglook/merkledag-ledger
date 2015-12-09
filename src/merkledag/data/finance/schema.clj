(ns merkledag.data.finance.schema
  "..."
  (:require
    [merkledag.link :as link]
    [schema.core :as s])
  (:import
    merkledag.link.MerkleLink
    (org.joda.time
      DateTime
      LocalDate)))


; TODO: move this somewhere else
(defrecord Quantity
  [value commodity])


(defn link-to
  "Schema for a merkle link to an object which must objey the given schema.
  Currently, the linked object is **NOT** recursively checked."
  [schema]
  ; TODO: implement recursive link schema
  MerkleLink)


(defn- constrained-keyword
  "Schema for a keyword with a mandatory namespace component."
  [ns]
  (s/constrained s/Keyword #(= ns (namespace %))))



;; ## Asset Classes

(def asset-classes
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


(def AssetClassKey
  "Schema for a keyword identifying an asset class."
  (s/named
    (constrained-keyword "finance.commodity.asset-class")
    "asset-class"))


(def AssetClassBreakdown
  "Schema for a map of asset classes to proportional numbers. The values in the
  map must sum to 1."
  (s/named
    (s/constrained
      {AssetClassKey s/Num}
      #(== 1 (reduce + (vals %))))
    "asset-class map"))



;; ## Commodity Sector

(def commodity-sectors
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


(def CommoditySectorKey
  "Schema for a keyword identifying a commodity sector."
  (constrained-keyword "finance.commodity.sector"))


(def CommoditySectorBreakdown
  "Schema for a map of sectors to proportional numbers. The values in the map
  must sum to 1."
  (s/named
    (s/constrained
      {CommoditySectorKey s/Num}
      #(== 1 (reduce + (vals %))))
    "sector map"))



;; ## Commodities

;; A commodity is defined by a symbolic code, a name, and a type. It may also
;; have a character symbol and a format example.

(def CommodityCode
  "Schema for a symbol identifying a commodity."
  (s/constrained s/Symbol #(re-matches #"[a-zA-Z][a-zA-Z0-9_]*" (str %))))


(def CommodityDefinition
  "Schema for a commodity definition directive."
  {:data/type (s/eq :finance/commodity)
   :title s/Str
   :finance.commodity/code
     CommodityCode
   (s/optional-key :finance.commodity/currency-symbol)
     (s/constrained s/Str #(= 1 (count %)))
   ; TODO: format?
   (s/optional-key :finance.commodity/asset-class)
     (s/conditional map? AssetClassBreakdown
                    :else AssetClassKey)
   (s/optional-key :finance.commodity/sector)
     (s/conditional map? CommoditySectorBreakdown
                    :else CommoditySectorKey)})



;; ## Prices

;; Over time, a commodity's value may change relative to other commodities. In
;; particular, currencies fluctuate and the values of investments grow and
;; shrink. The conversion rate between two commodities determines the primary
;; commodity's _price_ in the second (or 'base') commodity.

(def PriceEntry
  {:data/type (s/eq :finance/price)
   :time/at DateTime
   :finance.price/commodity CommodityCode
   :finance.price/value Quantity
   (s/optional-key :data/source) #{(link-to s/Any)}})



;; ## Accounts

;; Accounts are named containers for commodities. Accounts are structured into a
;; hierarchy, forming a tree of accounts. The top-level accounts should be
;; things like 'Assets', 'Liabilities', 'Income', 'Expenses', etc.
;;
;; GOALS:
;; - Be able to update account information and hierarchy WITHOUT needing to
;;   update the transaction history.
;; - Be able to move a subtree of accounts WITHOUT needing to update the accounts
;;   in the subtree.
;;
;; To this end, accounts are given a stable identifier by creating an
;; `account-root` structure and linking all postings and metadata to it. The
;; root data should include enough unique information to identify the
;; represented account.
;;
;; The accounts are organized into a tree from a top-level root of `/accounts`.
;; Each account gives its segment name (via `:title`) and links to any child
;; accounts.

(def account-types
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


(def AccountTypeKey
  "Schema for a keyword identifying an account type."
  (constrained-keyword "finance.account.type"))


(def AccountRoot
  "Schema for an object identifying the root of an account."
  {:data/type (s/eq :finance/account-root)
   :title s/Str
   (s/optional-key :description) s/Str
   (s/optional-key :time/at) DateTime})


(def AccountDefinition
  "Schema for an object defining the properties of an account."
  {:data/type (s/eq :finance/account)
   :title s/Str
   (s/optional-key :description) s/Str
   :finance.account/id (link-to AccountRoot)
   (s/optional-key :finance.account/type) AccountTypeKey
   (s/optional-key :finance.account/institution) (link-to s/Any)
   (s/optional-key :finance.account/external-id) s/Str
   (s/optional-key :finance.account/allowed-commodities) #{CommodityCode}
   (s/optional-key :finance.account/interest-rate) s/Num
   (s/optional-key :finance.account/children) #{(link-to AccountDefinition)}})



;; ## Line Items

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

(def LineItem
  "Schema for a line-item in an invoice."
  {:data/type (s/eq :finance/item)
   :title s/Str
   (s/optional-key :description) s/Str
   (s/optional-key :finance.item/id) s/Str
   (s/optional-key :finance.item/total) Quantity
   (s/optional-key :finance.item/amount)
     (s/conditional number? s/Num :else Quantity)
   (s/optional-key :finance.item/price) Quantity
   (s/optional-key :finance.item/vendor) (link-to s/Any)
   (s/optional-key :finance.item/tax-groups) #{s/Keyword}
   (s/optional-key :finance.item/tax-applied) s/Keyword}
  ; TODO: validations
  ; - amount and price only make sense if total is set
  ; - amount and price must be set together
  ; - total should equal amount * price (or be within tolerance)
  )


(def Invoice
  "Schema for a collection of line items in an invoice."
  {:data/type (s/eq :finance/invoice)
   (s/optional-key :title) s/Str
   (s/optional-key :description) s/Str
   (s/optional-key :data/sources) #{(link-to s/Any)}
   :finance.invoice/items [LineItem]})



;; ## Postings

;; An account has a _register_, which is the linear sequence of _postings_
;; applied to it. A posting is usually an amount change, but may also contain
;; balance checks and general notes. Postings are primarily sorted by timestamp,
;; but ones with the same timestamp (usually because of day-level precision) are
;; ordered by transaction placement in the history.

(def Posting
  "Schema for a financial posting to an account."
  {:data/type (s/eq :finance/posting)
   (s/optional-key :description) s/Str
   :finance.posting/account (link-to AccountRoot)
   (s/optional-key :finance.posting/id) s/Str
   (s/optional-key :finance.posting/type)
     (s/enum :finance.posting.type/real
             :finance.posting.type/virtual
             :finance.posting.type/balance-check)
   (s/optional-key :finance.posting/payee) (link-to s/Any)
   (s/optional-key :finance.posting/amount) Quantity
   (s/optional-key :finance.posting/price) Quantity
   (s/optional-key :finance.posting/lot-id) s/Str
   (s/optional-key :finance.posting/lot-cost) Quantity
   (s/optional-key :finance.posting/lot-date) LocalDate
   (s/optional-key :finance.posting/weight) Quantity
   (s/optional-key :finance.posting/balance) Quantity
   (s/optional-key :finance.posting/invoice) (link-to Invoice)
   (s/optional-key :time/at) DateTime
   (s/optional-key :data/sources) #{(link-to s/Any)}}
  ; TODO: validations
  ; - amount and price must have different commodities
  ; - weight only makes sense when price is specified
  ; - weight must be in same commodity as price
  ; - weight must be within tolerance of amount * price
  ; - balance should match commodity in amount
  ; - total of items in invoice must match amount
  ; - lot-id should specify a real previous posting
  ; - lot-cost and lot-date should match identified posting
  )



;; ## Transactions

;; Transactions tie together multiple _postings_ into a balanced unit. The
;; amounts within a transaction MUST sum to zero. Postings are applied in order
;; within the transaction.
;;
;; Transactions are organized into a _journals_ in a time-ordered sequence.
;; Each journal serves as a namespace for transactions, which can be combined
;; or viewed individually.
;;
;; - time      Time the transaction occurred.
;; - entries   Set of entries grouped into this transaction.
;; - state     Whether the transaction has cleared or is still pending.

; ...
