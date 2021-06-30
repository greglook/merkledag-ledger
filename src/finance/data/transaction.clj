(ns finance.data.transaction
  (:require
    [clojure.spec.alpha :as s]
    [datascript.core :as ds]
    [finance.data.book :as book]
    [finance.data.core :as data :refer [defattr defentity defref]]
    [finance.data.time :as time]))


;; ## Data Attributes

(defref ::book
  "Book the transaction belongs to."
  ::book/id)


(defident ::id
  "Unique identifier for the transaction."
  "txn")


(defattr ::date
  "Local calendar date on which the transaction occurred."
  ::time/local-date)


(defattr ::title
  "Title to display for the transaction."
  ::data/some-string)


(defattr ::description
  "Longer descriptive comment about the transaction."
  ::data/some-string)


(defattr ::flag
  "Optional flag value to apply to postings."
  #{:pending :cleared})


(defattr ::links
  "String identifiers linking transactions together."
  ;; TODO: not a collection?
  (s/coll-of string? :kind set?)
  :db/index true)


;; ## Normal Form

(defentity :finance.data/transaction
  :req [::book/id
        ::id
        ::date
        ::title]
  :opt [::description
        ::flag
        ::links])


;; ## Tree Form

(s/def ::entries
  (s/coll-of (data/tree-spec :finance.data/entry)
             :kind vector?))


(defmethod data/tree-form :finance.data/transaction
  [_]
  (s/keys :req [::date
                ::title
                ::entries]
          :opt [::id
                ::description
                ::flag
                ::links]))


(defmethod data/normalize-tree :finance.data/transaction
  [ctx txn]
  (let [entries (map-indexed
                  (fn [i entry]
                    (assoc entry :finance.data.entry/txn-rank i))
                  (::entries txn))
        txn (-> txn
                (dissoc ::entries)
                (assoc ::book/id (::book/id ctx)
                       ::id (or (::id txn) (gen-id))))
        ctx (assoc ctx :transaction txn)]
    (cons
      txn
      (mapcat
        (partial data/normalize-tree ctx)
        entries))))


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
              :where [[?a :finance.data.account/book ?book]
                      [?e :finance.data.entry/account ?a]
                      [?e :finance.data.entry/time ?time]
                      [?e :finance.data.entry/transaction ?tx]
                      [?tx :finance.data.transaction/date ?date]]}
           db book)
      (sort-by (comp vec rest))
      (map (comp (partial d/entity db) first)))))
