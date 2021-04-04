(ns finance.transaction
  (:require
    [clojure.spec :as s]
    [finance.entry :as entry]
    [finance.schema :refer [defattr defentity]]))


(defattr ::date
  "Local calendar date on which the transaction occurred."
  :time/date)


; TODO: non-virtual postings must sum to zero
(defattr ::entries
  "References to child journal entries."
  (s/coll-of :finance/entry :kind vector? :min-count 1)
  :db/cardinality :db.cardinality/many)


(defattr ::links
  "String identifiers linking transactions together."
  (s/coll-of string? :kind set?)
  :db/index true)


(defattr ::flag
  "Optional flag value to apply to postings."
  #{:pending :cleared})


(defentity :finance/transaction
  "..."
  :req [:data/title
        ::date
        ::entries]
  :opt [:data/description
        :data/tags
        :time/at
        ::links
        ::flag])
