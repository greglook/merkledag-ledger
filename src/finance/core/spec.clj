(ns finance.core.spec
  "Schema definitions for entities in the financial system."
  (:require
    [clojure.set :as set]
    [clojure.spec :as s]
    [clojure.spec.gen :as gen]
    [finance.core.types :refer [q quantity?]]
    [merkledag.link :as link])
  (:import
    (org.joda.time
      DateTime
      Interval
      LocalDate)))


(def attributes
  "Map of all defined attributes and their datascript definitions."
  {})


(defmacro defattr
  [attr-key doc-str spec & {:as opts}]
  `(do
     (s/def ~attr-key ~spec)
     (alter-var-root #'attributes assoc ~attr-key ~(assoc opts :db/doc doc-str))))


(defmacro defentity
  [type-key doc-str & {:as key-args}]
  `(s/def ~type-key
     (let [spec# (s/keys ~@(apply concat key-args))]
       (s/with-gen
         (s/and spec# (comp #{~type-key} :data/type))
         #(gen/fmap
            (fn [e#] (assoc e# :data/type ~type-key))
            (s/gen spec#))))))



;; ## General Data Attributes

(defn merkle-link?
  "Predicate which returns true if the value `x` is a merkle link object."
  [x]
  (instance? merkledag.link.MerkleLink x))


(defattr :data/ident
  "Unique identifier for data entities."
  string?
  :db/unique :db.unique/value)


(defattr :data/type
  "Keyword identifying the primary entity type."
  (s/and keyword? namespace)
  :db/index true)


(defattr :data/title
  "Title to present the data value with."
  string?)


(defattr :data/description
  "Human-readable description string."
  string?)


(defattr :data/sources
  "Set of links to source documents the entity is constructed from."
  (s/coll-of merkle-link? :kind set? :min-count 1)
  ;:db/valueType :db.type/ref
  :db/cardinality :db.cardinality/many)


(defattr :data/tags
  "Map of keyword tags to string values. Primarily used for indexing related
  entities."
  (s/map-of keyword? string?))



;; ## Time Attributes

(defattr :time/at
  "Instant in time at which the data occurred."
  #(instance? DateTime %))


(defattr :time/interval
  "Interval in time the data occurred over."
  #(instance? Interval %))



;; ## Asset Properties

(def asset-types
  "Set of names for some common asset types."
  #{:currency
    :bond
    :stock
    :mutual-fund
    :exchange-traded-fund
    :reward-points})

; TODO: record asset class tree structure

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


(def asset-sectors
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


(defn- distribution-map
  "Creates a spec for a key type which can either be a single keyword or a map
  of keys whose values sum to one."
  [key-spec]
  (s/or
    :single key-spec
    :multi (s/with-gen
             (s/and (s/map-of key-spec (s/double-in :min 0.0
                                                    :max 1.0
                                                    :infinite? false
                                                    :NaN? false))
                    #(== 1 (reduce + (vals %))))
             #(gen/fmap
                (fn [m]
                  (if (= 1 (count m))
                    (key (first m))
                    (let [total (reduce + (vals m))]
                      (into {} (map (fn [[k v]] [k (/ v total)]) m)))))
                (gen/map (s/gen key-spec) (s/gen (s/and float? pos?)))))))


(defattr :finance.asset/type
  "Type of value that this asset represents."
  asset-types)


(defattr :finance.asset/class
  "Map of asset class breakdowns or single class keyword."
  (distribution-map asset-classes))


(defattr :finance.asset/sector
  "Map of asset sector breakdowns or single sector keyword."
  (distribution-map asset-sectors))



;; ## Commodities

(defattr :finance.commodity/code
  "Code symbol used to identify the commodity."
  (s/with-gen
    (s/and symbol? #(re-matches #"[a-zA-Z][a-zA-Z0-9_]*" (str %)))
    #(let [number-chars (set (map char (range 48 58)))
           upper-chars (set (map char (range 65 91)))
           lower-chars (set (map char (range 97 123)))
           prefix-chars (set/union upper-chars lower-chars)
           body-chars (set/union number-chars upper-chars lower-chars #{\_})]
       (gen/fmap
         (fn [[prefix body]]
           (symbol (apply str prefix body)))
         (gen/tuple (gen/elements prefix-chars)
                    (gen/vector (gen/elements body-chars))))))
  :db/unique :db.unique/identity)


(defattr :finance.commodity/symbol
  "One-character string to prefix currency amounts with."
  (s/with-gen
    (s/and string? #(= 1 (count %)))
    #(gen/fmap str (gen/char-ascii))))


; TODO: clarify the relation between precision and tolerance
(defattr :finance.commodity/precision
  "Number of decimal places to represent the commodity to."
  integer?)


(defentity :finance/commodity
  "..."
  :req [:data/title
        :finance.commodity/code
        :finance.asset/type]
  :opt [:data/description
        :finance.commodity/symbol
        :finance.commodity/precision
        :finance.asset/class
        :finance.asset/sector])



;; ## Financial Quantities

(s/def :finance/quantity
  (s/with-gen
    (s/and quantity?
           (comp number? :amount)
           (comp symbol? :commodity))
    #(gen/fmap
       (fn [[base exp code]]
         (q (nth (iterate (fn [x] (/ x 10)) (bigdec base))
                 exp)
            code))
       (gen/tuple (gen/large-integer)
                  (gen/large-integer* {:min 0, :max 3})
                  (s/gen :finance.commodity/code)))))



;; ## Prices and Lots

(defattr :finance.price/commodity
  "Commodity the price is measuring."
  :finance.commodity/code
  :db/valueType :db.type/ref)


(defattr :finance.price/value
  "Amount of the base commodity a unit of this commodity costs."
  :finance/quantity)


(defentity :finance/price
  "..."
  :req [:finance.price/commodity
        :finance.price/value
        :time/at])



;; ## Items and Invoices

(defattr :finance.item/total
  "Total amount contributed by this item."
  :finance/quantity)


(defattr :finance.item/amount
  "Amount of the item on the invoice. A bare number indicates a unitless amount
  of items transacted."
  (s/or :count number? :quantity :finance/quantity))


(defattr :finance.item/price
  "Price per unit of the item. A bare number is treated as a unit percentage
  multiplier."
  (s/or :percentage number? :quantity :finance/quantity))


(defattr :finance.item/vendor
  "Additional string describing the vendor the item is from."
  string?)


(defattr :finance.item/tax-groups
  "Set of keywords indicating the tax groups a given item is part of."
  (s/coll-of keyword? :kind set?))


(defattr :finance.item/tax-applied
  "Keyword indicating the group this tax item applies to."
  keyword?)


; TODO: validations
; - amount and price only make sense if total is set
; - amount and price must be set together
; - total should equal amount * price (or be within tolerance)
(defentity :finance/item
  "..."
  :req [:data/title]
  :opt [:data/description
        :finance.item/total
        :finance.item/amount
        :finance.item/price
        :finance.item/vendor
        :finance.item/tax-groups
        :finance.item/tax-applied])


(defattr :finance.invoice/items
  "Collection of items that make up the invoice."
  (s/coll-of :finance/item :kind vector?)
  :db/valueType :db.type/ref
  :db/cardinality :db.cardinality/many)


(defentity :finance/invoice
  "..."
  :req [:finance.invoice/items])



;; ## Accounts

(def account-types
  "Set of names for some common account types."
  #{:cash
    :savings
    :checking
    :demand-deposit
    :certificate-of-deposit
    :brokerage
    :traditional-401k
    :roth-401k
    :traditional-ira
    :roth-ira
    :health-savings
    :credit-card
    :mortgage
    :student-loan
    :loan
    :reward-program
    :bitcoin
    :property})


(defattr :finance.account/id
  "Link to the account's root node."
  merkle-link?
  :db/unique :db.unique/identity)


(defattr :finance.account/book
  "Name of the book the account is part of."
  string?)


(defattr :finance.account/path
  "Path segments to uniquely identify the account within a book."
  (s/+ string?)
  :db/index true)


(defattr :finance.account/alias
  "Keyword alias to refer to the account by."
  keyword?
  :db/index true)


(defattr :finance.account/type
  "Keyword identifying the type of account."
  account-types)


(defattr :finance.account/external-id
  "String giving the account's external identifier, such as an account number."
  string?
  :db/unique :db.unique/identity)


(defattr :finance.account/commodities
  "Set of commodities which are valid for the account to contain."
  (s/coll-of :finance.commodity/code :kind set?)
  :db/cardinality :db.cardinality/many)


(defattr :finance.account/links
  "String identifiers linking related accounts together."
  string?
  :db/cardinality :db.cardinality/many
  :db/index true)


(defentity :finance/account
  "..."
  :req [:finance.account/id
        :finance.account/book
        :finance.account/path]
  :opt [:data/title
        :data/description
        :finance.account/alias
        :finance.account/type
        :finance.account/external-id
        :finance.account/commodities
        :finance.account/links])



;; ## Journal Entries

(defattr :finance.entry/account
  "Name of the account the entry is related to."
  (s/or :path :finance.account/path
        :alias :finance.account/alias)
  :db/valueType :db.type/ref)


(defattr :finance.entry/source-lines
  "Set of lines pulled from third-party sources that this entry represents."
  (s/coll-of (s/cat :source-tag keyword? :line string?) :kind set?)
  :db/cardinality :db.cardinality/many)


(defattr :finance.entry/external-id
  "String containing an external identifier for the entry, for deduplication."
  string?
  :db/index true)


(defattr :finance.entry/rank
  "Extra numeric value to determine the ordering of entries within an account
  register which have the same timestamp."
  number?)


(defmulti journal-entry :data/type)


(s/def :finance/entry
  (s/multi-spec journal-entry :data/type))



;; ### Notes

(defentity :finance.entry/note
  "..."
  :req [:data/description
        :time/at
        :finance.entry/account]
  :opt [:time/interval
        :finance.entry/source-lines
        :finance.entry/external-id
        :finance.entry/rank])


(defmethod journal-entry :finance.entry/note
  [_]
  :finance.entry/note)


;; ### Lifecycle Markers

(defentity :finance.entry/open-account
  "..."
  :req [:time/at
        :finance.entry/account]
  :opt [:data/description
        :finance.entry/source-lines
        :finance.entry/external-id
        :finance.entry/rank])


(defmethod journal-entry :finance.entry/open-account
  [_]
  :finance.entry/open-account)


(defentity :finance.entry/close-account
  "..."
  :req [:time/at
        :finance.entry/account]
  :opt [:data/description
        :finance.entry/source-lines
        :finance.entry/external-id
        :finance.entry/rank])


(defmethod journal-entry :finance.entry/close-account
  [_]
  :finance.entry/close-account)




;; ### Balance Assertions

(defattr :finance.balance/amount
  "Amount of a certain commodity the account should contain."
  :finance/quantity)


(defentity :finance.entry/balance-check
  "..."
  :req [:time/at
        :finance.entry/account
        :finance.balance/amount]
  :opt [:data/description
        :finance.entry/source-lines
        :finance.entry/external-id
        :finance.entry/rank])


(defmethod journal-entry :finance.entry/balance-check
  [_]
  :finance.entry/balance-check)



;; ## Lots and Positions

(defattr :finance.lot/amount
  "Quantity of the commodity paid for this lot."
  :finance/quantity)


(defattr :finance.lot/date
  "Calendar date associated with the lot."
  #(instance? LocalDate %))


(defentity :finance/lot
  "..."
  :req [:finance.lot/amount]
  :opt [:finance.lot/date
        :data/title])



;; ## Postings

(defattr :finance.posting/virtual
  "Boolean flag indicating that the posting is virtual and need not balance."
  boolean?)


(defattr :finance.posting/payee
  "String name for the counterparty of this posting."
  string?)


(defattr :finance.posting/amount
  "Quantity of a commodity that is changed in the account."
  :finance/quantity)


(defattr :finance.posting/price
  "Price per-unit of the commodity in `amount` the posting took place at."
  :finance/quantity)


(defattr :finance.posting/weight
  "If `price` is set, rather than relying on multiplying the amount by the
  price, an explicit balance weight can be given."
  :finance/quantity)


(defattr :finance.posting/cost
  "Reference to the posting which established the position this posting is altering"
  merkle-link?
  :db/valueType :db.type/ref)


(defattr :finance.posting/invoice
  "Reference to an itemized list for the posting amount."
  merkle-link?
  :db/valueType :db.type/ref)


(defentity :finance.entry/posting
  "..."
  :req [:time/at
        :finance.entry/account]
  :opt [:data/description
        :finance.entry/source-lines
        :finance.entry/external-id
        :finance.entry/rank
        :finance.balance/amount])


(defmethod journal-entry :finance.entry/posting
  [_]
  :finance.entry/posting)



;; ## Transactions

(defattr :finance.transaction/date
  "Local calendar date on which the transaction occurred."
  #(instance? LocalDate %))


; TODO: non-virtual postings must sum to zero
(defattr :finance.transaction/entries
  "References to child journal entries."
  (s/coll-of :finance/entry :kind vector? :min-count 1)
  :db/cardinality :db.cardinality/many)


(defattr :finance.transaction/links
  "String identifiers linking transactions together."
  (s/coll-of string? :kind set?)
  :db/index true)


(defattr :finance.transaction/flag
  "Optional flag value to apply to postings."
  #{:pending :cleared})


(defentity :finance/transaction
  "..."
  :req [:data/title
        :finance.transaction/date
        :finance.transaction/entries]
  :opt [:data/description
        :data/tags
        :time/at
        :finance.transaction/links
        :finance.transaction/flag])
