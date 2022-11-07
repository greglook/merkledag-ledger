(ns finance.data.budget
  (:require
    [clojure.spec.alpha :as s]
    [finance.data.account :as account]
    [finance.data.commodity :as commodity]
    [finance.data.core :refer [defattr defident defentity]]))


;; ## Data Attributes

(defident ::id
  "Unique identifier for the budget pool."
  "bdg")


(defattr ::title
  "Descriptive title for the budget."
  string?)


(defattr ::description
  "Longer form human description for the budget."
  string?)


(defattr ::commodity
  "The commodity this budget is measured in."
  ::commodity/code)


#_
(defattr ::balance
  "Current value of the budget."
  number?)


(defattr ::funding-rate
  "How much value to allocate per ???."
  number?)


(defattr ::limit
  "The maximum amount of value to allocate to this budget."
  number?)


;; ## Normal Form

(defentity :finance.data/budget
  :req [::id
        ::title
        ::commodity]
  :opt [::description
        ::funding-rate
        ::limit])


;; TODO: how to represent balance history?
