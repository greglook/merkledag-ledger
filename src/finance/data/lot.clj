(ns finance.data.lot
  (:require
    [finance.data.core :refer [defattr defentity]]
    [finance.data.quantity :as quantity]
    [finance.data.time :as time]))


(defattr ::amount
  "Quantity of the commodity paid for this lot."
  ::quantity/q)


(defattr ::date
  "Calendar date associated with the lot."
  ::time/local-date)


(defentity :finance.data/lot
  "A single lot making up part of a position."
  ;; TODO: seems like this is missing another amount
  :req [::amount]
  :opt [::date])
