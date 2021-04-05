(ns finance.data.transaction
  (:require
    [clojure.spec.alpha :as s]
    [datascript.core :as ds]
    [finance.data.book :as book]
    [finance.data.entry :as entry]
    [finance.data.core :refer [defattr defentity]]
    [finance.data.time :as time]))


;; ## Data Specs

(defattr ::date
  "Local calendar date on which the transaction occurred."
  ::time/local-date)


;; TODO: non-virtual postings must sum to zero
(defattr ::entries
  "References to child journal entries."
  (s/coll-of :finance.data/entry :kind vector? :min-count 1)
  :db/cardinality :db.cardinality/many)


(defattr ::links
  "String identifiers linking transactions together."
  (s/coll-of string? :kind set?)
  :db/index true)


(defattr ::flag
  "Optional flag value to apply to postings."
  #{:pending :cleared})


(defentity :finance.data/transaction
  "..."
  :req [::book/id
        ::date
        ::title
        ::entries]
  :opt [::time
        ::description
        ::links
        ::flag])



;; ## Functions

; Static checks:
; - must be at least one entry
; - all entry accounts belong to the same books
; - real posting weights must sum to zero

; Historical checks:
; ...

#_
(defn select-transactions
  [db book & query-args]
  (let [query (if (and (= 1 (count query-args))
                       (map? query-args))
                query-args
                (apply hash-map query-args))]
    (->>
      ; TODO: figure out what query args are suitable
      ; minimally, filtering on date should work
      (ds/q '{:find [?tx ?date (min ?time)]
              :in [$ ?book]
              :where [[?a :finance.account/book ?book]
                      [?e :finance.entry/account ?a]
                      [?e :time/at ?time]
                      [?tx :finance.transaction/entries ?e]
                      [?tx :finance.transaction/date ?date]]}
           db book)
      (sort-by (comp vec rest))
      (map (comp (partial d/entity db) first)))))
