(ns finance.data.budget
  (:require
    [clojure.spec.alpha :as s]
    [finance.data.account :as account]
    [finance.data.commodity :as commodity]
    [finance.data.core :refer [defattr defentity]]))


(defattr ::commodity
  "..."
  ::commodity/code)


(defattr ::balance
  "..."
  number?)


(defattr ::max-balance
  "..."
  number?)


(defattr ::funding-rate
  "..."
  number?)


(defattr ::account-affinities
  "..."
  (s/coll-of ::account/id :kind vector?))


;; TODO: how to represent balance history?
(defentity :finance.data/budget
  "..."
  :req [:data/title
        ::commodity
        ::balance
        ::max-balance
        ::funding-rate]
  :opt [:data/description
        :data/tags
        ::account-affinities])
