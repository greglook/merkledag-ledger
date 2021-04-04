(ns finance.data.price
  (:require
    [finance.data.commodity :as commodity]
    [finance.data.core :refer [defattr defentity]]
    [finance.data.time :as time]))


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
