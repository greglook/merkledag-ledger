(ns finance.data.price
  (:require
    [datascript.core :as ds]
    [finance.data.commodity :as commodity]
    [finance.data.core :refer [defattr defentity]]
    [finance.data.quantity :as quantity]
    [finance.data.time :as time]))


;; ## Data Specs

(defattr ::time
  "Time the price was observed."
  ::time/instant)


(defattr ::commodity
  "Commodity the price is measuring."
  ::commodity/code
  :db/valueType :db.type/ref)


(defattr ::value
  "Amount of the base commodity a unit of this commodity costs."
  ::quantity/q)


(defentity :finance.data/price
  "A point in time establishing the price of one commodity in another."
  :req [::time
        ::commodity
        ::value])



;; ## Functions

; Static checks:
; - schema validates
;
; Historical checks:
; - check for duplicate prices?

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
