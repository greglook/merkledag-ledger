(ns finance.data.price
  "A point in time establishing the price of one commodity in another."
  (:require
    [datascript.core :as ds]
    [finance.data.commodity :as commodity]
    [finance.data.core :as data :refer [defattr defref defentity]]
    [finance.data.quantity :as quantity]
    [finance.data.time :as time]))


;; ## Data Attributes

;; TODO: are prices linked directly to books, or are they linked indirectly via local commodities?
(defref ::commodity
  "Commodity the price is measuring."
  ::commodity/code)


(defattr ::time
  "Time the price was observed."
  ::time/instant)


(defattr ::value
  "Amount of the quantified commodity a unit of this commodity costs."
  ::quantity/q)


;; TODO: some notion of data source


;; ## Normal Form

(defentity :finance.data/price
  :req [::commodity/code
        ::time
        ::value])


;; ## Tree Form

(defmethod data/tree-form :finance.data/price
  [_]
  (s/keys :req [::commodity/code
                ::time
                ::value]))


(defmethod data/normalize-tree :finance.data/price
  [ctx price]
  [(assoc price ::commodity/code (::commodity/code ctx))])


;; ## Functions

; Static checks:
; - schema validates
;
; Historical checks:
; - check for duplicate prices?

#_
(defn find-duplicate-prices
  [db]
  (ds/q '[:find ?p1 ?p2
          :where
          [(< ?p1 ?p2)]
          [?p1 ::commodity ?c]
          [?p2 ::commodity ?c]
          [?p1 ::time ?time]
          [?p2 ::time ?time]]
        db))
