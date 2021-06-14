(ns finance.data.item
  (:require
    [clojure.spec.alpha :as s]
    [finance.data.core :refer [defattr defentity defref]]
    [finance.data.entry :as entry]
    [finance.data.invoice :as invoice]
    [finance.data.quantity :as quantity]))


(defref ::invoice
  "Invoice the item is part of."
  ::invoice/id)


(defref ::entry
  "Transaction entry the item contributes to."
  ::entry/id)


(defattr ::title
  "Human-friendly title for the item."
  string?)


(defattr ::description
  "Additional descriptive information about the item."
  string?)


(defattr ::total
  "Total amount contributed by this item."
  ::quantity/q)


(defattr ::amount
  "Amount of the item on the invoice. A bare number indicates a unitless amount
  of items transacted."
  (s/or :count number?
        :quantity ::quantity/q))


(defattr ::price
  "Price per unit of the item. A bare number is treated as a unit percentage
  multiplier."
  (s/or :percentage number?
        :quantity ::quantity/q))


(defattr ::vendor
  "Additional string describing the vendor the item is from."
  string?)


(defattr ::tax-groups
  "Set of keywords indicating the tax groups a given item is part of."
  (s/coll-of keyword? :kind set?))


(defattr ::tax-applied
  "Keyword indicating the group this tax item applies to."
  keyword?)


(defentity :finance.data/item
  "An individual item in an invoice of detailed charges."
  :req [::title]
  :opt [::description
        ::total
        ::amount
        ::price
        ::vendor])


;; TODO: validations
;; - amount and price must be set together
;; - amount and price only make sense if total is set
;; - total should equal amount * price (or be within tolerance)
