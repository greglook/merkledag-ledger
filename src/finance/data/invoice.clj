(ns finance.data.invoice
  "An invoice is an itemized list of charges, with information about each item
  such as a description, quantity, cost, etc."
  (:require
    [clojure.spec.alpha :as s]
    [finance.data.book :as book]
    [finance.data.core :as data :refer [defattr defentity defref]]))


(defattr ::id
  "Unique identifier for an invoice."
  ::data/some-string
  :db/unique :db.unique/identity)


;; TODO: other invoice attributes; should be able to independently model a receipt


;; TODO: how does this stay ordered?
(defattr ::items
  "Collection of items that make up the invoice."
  (s/coll-of :finance.data/item :kind vector?)
  :db/valueType :db.type/ref
  :db/cardinality :db.cardinality/many)


(defentity :finance.data/invoice
  "An invoice of items."
  :req [::id])
