(ns finance.data.posting
  "Postings are the main entry type, which modify the quantity of a commodity
  within the account."
  (:require
    [finance.data.balance :as balance]
    [finance.data.core :as data :refer [defattr]]
    [finance.data.entry :as entry :refer [defentry]]
    [finance.data.quantity :as quantity]))


(defattr ::id
  "Unique stable identifier for this posting."
  ::data/some-string
  :db/unique :db.unique/identity)


(defattr ::virtual?
  "Boolean flag indicating that the posting is virtual and need not balance."
  boolean?)


(defattr ::payee
  "String name for the counterparty of this posting."
  string?)


(defattr ::amount
  "Quantity of a commodity that is changed in the account."
  ::quantity/q)


(defattr ::price
  "Price per-unit of the commodity in `amount` the posting took place at."
  ::quantity/q)


(defattr ::weight
  "If `price` is set, rather than relying on multiplying the amount by the
  price, an explicit balance weight can be given."
  ::quantity/q)


(defattr ::cost
  "Reference to the posting which established the position this posting is altering"
  ::id
  :db/valueType :db.type/ref)


(defentry :posting
  "Postings modify commodity amounts in accounts."
  :req [::id
        ::amount]
  :opt [::price
        ::weight
        ::cost
        ::virtual?
        ::payee
        ::balance/amount])
