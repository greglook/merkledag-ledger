(ns finance.data.commodity
  "A commodity defines a type of value storage and exchange."
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as str]
    [finance.data.book :as book]
    [finance.data.core :as data :refer [defattr defentity]]))


;; ## Asset Information

(def asset-types
  "Set of names for some common asset types."
  #{:currency
    :bond
    :stock
    :mutual-fund
    :exchange-traded-fund
    :reward-points})


(def asset-classes
  "Set of names for some common asset classes."
  #{:cash
    :intl-government-bond
    :intl-corporate-bond
    :us-government-bond
    :us-corporate-bond
    :us-municipal-bond
    :p2p-lending
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


(def asset-class-tree
  "Vector tree representing the asset class hierarchy."
  [:all
   [:cash]
   [:fixed-income
    [:intl-bonds
     [:intl-government-bond]
     [:intl-corporate-bond]]
    [:us-bonds
     [:us-government-bond]
     [:us-corporate-bond]
     [:us-municipal-bond]]
    [:p2p-lending]]
   [:equities
    [:intl-stocks
     [:intl-developed-stock]
     [:intl-emerging-stock]]
    [:us-stocks
     [:us-large-cap
      [:us-large-cap-value]
      [:us-large-cap-core]
      [:us-large-cap-growth]]
     [:us-mid-cap
      [:us-mid-cap-value]
      [:us-mid-cap-core]
      [:us-mid-cap-growth]]
     [:us-small-cap
      [:us-small-cap-value]
      [:us-small-cap-core]
      [:us-small-cap-growth]]]]
   [:alternatives
    [:currency]
    [:real-estate]
    [:gold]
    [:commodities]
    [:other]]])


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


(defn- distribution-map-spec
  "Creates a spec for a map of keys whose values sum to one."
  [key-spec]
  (s/with-gen
    (s/and (s/map-of key-spec
                     (s/double-in :min 0.0
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
       (gen/map (s/gen key-spec)
                (gen/double*
                  {:min 0.0
                   :infinite? false
                   :NaN? false})
                {:min-elements 1
                 :max-elements 10}))))


(defn- distribution-spec
  "Creates a spec for a key type which can either be a single keyword or a map
  of keys whose values sum to one."
  [key-spec]
  (s/or
    :single key-spec
    :multi (distribution-map-spec key-spec)))


;; ## Data Attributes

(defref ::book
  "The book this commodity is local to, if applicable."
  ::book/id)


(defattr ::code
  "Code symbol used to identify the commodity."
  (s/with-gen
    (s/and symbol? #(re-matches #"[a-zA-Z][a-zA-Z0-9_]*" (str %)))
    (let [number-chars (set (map char (range 48 58)))
          upper-chars (set (map char (range 65 91)))
          lower-chars (set (map char (range 97 123)))
          prefix-chars (set/union upper-chars lower-chars)
          body-chars (set/union number-chars upper-chars lower-chars #{\_})]
      #(gen/fmap
         (fn [[prefix body]]
           (symbol (apply str prefix body)))
         (gen/tuple (gen/elements prefix-chars)
                    (gen/vector (gen/elements body-chars))))))
  :db/unique :db.unique/identity)


(defattr ::name
  "Human-meaningful name of the commodity."
  ::data/some-string)


(defattr ::type
  "Type of value that this asset represents."
  asset-types)


(defattr ::description
  "Long-form descriptive text for the commodity."
  string?)


(defattr ::currency-symbol
  "One-character string to prefix currency amounts with."
  (s/with-gen
    (s/and string? #(= 1 (count %)))
    #(gen/fmap str (gen/char-ascii))))


;; TODO: clarify the relation between precision and tolerance
;; TODO: rename 'scale'?
;; TODO: this should also not prevent higher-precision values from appearing e.g. as commodity prices
(defattr ::precision
  "Default number of decimal places to represent the commodity to."
  nat-int?)


(defattr ::asset-class
  "Map of asset class breakdowns or single class keyword."
  (distribution-spec asset-classes))


(defattr ::asset-sector
  "Map of asset sector breakdowns or single sector keyword."
  (distribution-spec asset-sectors))


;; ## Normal Form

(defentity :finance.data/commodity
  :req [::code
        ::name]
  :opt [::book/id
        ::type
        ::description
        ::currency-symbol
        ::precision
        ::asset-class
        ::asset-sector])


;; ## Tree Form

(defmethod data/tree-form :finance.data/commodity
  [_]
  (s/keys :req [::code
                ::name]
          :opt [::type
                ::description
                ::currency-symbol
                ::precision
                ::asset-class
                ::asset-sector]))


(defmethod data/normalize-tree :finance.data/commodity
  [ctx commodity]
  [commodity])
