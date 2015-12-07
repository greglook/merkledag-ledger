(ns merkledag.data.finance.schema
  "..."
  (:require
    [schema.core :as s]))


;; ## Asset Classes

(def asset-classes
  "Set of standard asset classes."
  #{{:data/ident :finance.commodity.asset-class/cash
     :title "Cash"
     :description "Cash and other cash-equivalent securities."}
    {:data/ident :finance.commodity.asset-class/intl-government-bond
     :title "International Government Bonds"}
    {:data/ident :finance.commodity.asset-class/intl-corporate-bond
     :title "International Corporate Bonds"}
    {:data/ident :finance.commodity.asset-class/us-government-bond
     :title "US Government Bonds"}
    {:data/ident :finance.commodity.asset-class/us-corporate-bond
     :title "US Corporate Bonds"}
    {:data/ident :finance.commodity.asset-class/us-municipal-bond
     :title "US Municipal Bonds"}
    {:data/ident :finance.commodity.asset-class/intl-developed-stock
     :title "International Developed Stock"}
    {:data/ident :finance.commodity.asset-class/intl-emerging-stock
     :title "International Emerging Stock"}
    {:data/ident :finance.commodity.asset-class/us-large-cap-value
     :title "US Large Cap Value Stock"}
    {:data/ident :finance.commodity.asset-class/us-large-cap-core
     :title "US Large Cap Core Stock"}
    {:data/ident :finance.commodity.asset-class/us-large-cap-growth
     :title "US Large Cap Growth Stock"}
    {:data/ident :finance.commodity.asset-class/us-mid-cap-value
     :title "US Mid Cap Value Stock"}
    {:data/ident :finance.commodity.asset-class/us-mid-cap-core
     :title "US Mid Cap Core Stock"}
    {:data/ident :finance.commodity.asset-class/us-mid-cap-growth
     :title "US Mid Cap Growth Stock"}
    {:data/ident :finance.commodity.asset-class/us-small-cap-value
     :title "US Small Cap Value Stock"}
    {:data/ident :finance.commodity.asset-class/us-small-cap-core
     :title "US Small Cap Core Stock"}
    {:data/ident :finance.commodity.asset-class/us-small-cap-growth
     :title "US Small Cap Growth Stock"}
    {:data/ident :finance.commodity.asset-class/currency
     :title "Currencies"
     :description "Alternative currencies held speculatively."}
    {:data/ident :finance.commodity.asset-class/real-estate
     :title "Real Estate"
     :description "Direct real-estate investments or diversified REITs."}
    {:data/ident :finance.commodity.asset-class/gold
     :title "Gold"}
    {:data/ident :finance.commodity.asset-class/commodities
     :title "Commodities"}
    {:data/ident :finance.commodity.asset-class/other
     :title "Other Assets"
     :description "Catch-all for unclassified assets."}})


(def AssetClassKey
  "Enumeration of keywords identifying an asset class."
  (s/named
    (apply s/enum (map :data/ident asset-classes))
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

(def CommoditySectorKey
  "Enumeration of keywords identifying a commodity sector."
  (s/enum
    :finance.commodity.sector/basic-materials
    :finance.commodity.sector/communication-services
    :finance.commodity.sector/consumer-cyclical
    :finance.commodity.sector/consumer-defensive
    :finance.commodity.sector/energy
    :finance.commodity.sector/financial-services
    :finance.commodity.sector/healthcare
    :finance.commodity.sector/industrials
    :finance.commodity.sector/technology
    :finance.commodity.sector/utilities))


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
;;
;; Commodity definitions are stored under `/commodities`, which should be a
;; link table from code symbols to definitions. Thus, the definition of US
;; dollars could be found at `/commodities/USD`.

(def CommodityCode
  (s/constrained s/Symbol #(re-matches #"[a-zA-Z][a-zA-Z0-9_]*" (str %))))


(def CommodityDefinition
  {:type :finance/commodity
   :title s/Str
   :finance.commodity/code
     CommodityCode
   (s/optional-key :finance.commodity/currency-symbol)
     (s/constrained s/String #(= 1 (count %)))
   (s/optional-key :finance.commodity/asset-class)
     (s/conditional map? AssetClassBreakdown
                    :else AssetClassKey)
   (s/optional-key :finance.commodity/sector)
     (s/conditional map? CommoditySectorBreakdown
                    :else CommoditySectorKey)
   ; TODO: format?
   })



;; ## Prices

;; Over time, a commodity's value may change relative to other commodities. In
;; particular, currencies fluctuate and the values of investments grow and
;; shrink. The conversion rate between two commodities determines the primary
;; commodity's _price_ in the second (or 'base') commodity.
;;
;; These are stored in a 'price-list' structure, which is a sequence of
;; maps containing the following keys:
;; - at*         time the price point was observed
;; - commodity*  the primary commodity being priced
;; - price*      unit value of the primary commodity in the base units
;; - source      string describing the source of the data

; ...



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
;; `open-account` structure and linking all postings and metadata to it. The
;; opening data should include enough unique information to identify the
;; represented account.
;;
;; The accounts are organized into a tree from a top-level root of `/accounts`.
;; Each account gives its segment name (via `:title`) and links to any child
;; accounts.
;;
;; - id              link to the opening event (for accounts with a journal)
;; - type*           type of account, e.g. :group, :checking, :savings, :brokerage, etc.
;; - institution     reference to the entity the account is with, if any
;; - external-id     external account identifier string
;; - commodities     set of commodity codes allowed in the account, if restricted
;; - interest-rate   APR paid/charged, if any
;; - children        set of links to child accounts

; ...



;; ## Line Items

;; A line item represents a transacted amount of a product at a certain price.
;; A common example is a line on a receipt, showing the purchase of an item.
;;
;; Typically, the _amount_ will be a bare number or a physical quantity. The
;; _price_ is the unit price of the item, as a financial quantity. The _total_
;; multiplies the two and takes the commodity specified in the price. This is
;; rendered like:
;;
;;     $20.18 (2.0 lb @ $10.09)
;;
;; When the amount is a financial quantity and the price is a regular number,
;; it's considered to be a percentage of the amount. One common example is tax,
;; which is applied as a fraction of the bill total. This is rendered like:
;;
;;     $12.22 ($127.29 @ 9.6%)

; ...



;; ## Postings

;; An account has a _register_, which is the linear sequence of _postings_
;; applied to it. A posting is usually an amount change, but may also contain
;; balance checks and general notes. Postings are primarily sorted by timestamp,
;; but ones with the same timestamp (usually because of day-level precision) are
;; ordered by transaction placement in the history.
;;
;; - id            optional string uniquely identifying this posting so it can be
;;                 linked to another posting
;; - account*      Reference to the account (via the opening data) the posting
;;                 should be applied to.
;; - payee         Link to the entity on the other side of the posting, if any.
;; - balances      A balance check asserts that an account has a certain balance
;;                 of some set of commodities. If a commodity is not present in
;;                 the balance map, it is not checked. To assert that an account
;;                 has none of a given commodity, it should be specified with a
;;                 value of zero.
;; - amount        Quantity of commodity which changed.
;; - price         A price in another commodity which the amount was exchanged for.
;; - cost          A cost associated with the original commodity lot.
;; - items         A list of itemized amounts that contribute to the posting.

; ...



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
