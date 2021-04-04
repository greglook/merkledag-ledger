(ns finance.data.asset
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [finance.data.core :refer [defattr]]))


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


(defn- distribution-map
  "Creates a spec for a key type which can either be a single keyword or a map
  of keys whose values sum to one."
  [key-spec]
  (s/or
    :single key-spec
    :multi (s/with-gen
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
                         (s/double-in :min 0.0
                                      :infinite? false
                                      :NaN? false)
                         {:min-elements 1
                          :max-elements 10})))))


;; TODO: eventually open up these specs to user extension by allowing any
;; keyword.

(defattr ::type
  "Type of value that this asset represents."
  asset-types)


(defattr ::class
  "Map of asset class breakdowns or single class keyword."
  (distribution-map asset-classes))


(defattr ::sector
  "Map of asset sector breakdowns or single sector keyword."
  (distribution-map asset-sectors))
