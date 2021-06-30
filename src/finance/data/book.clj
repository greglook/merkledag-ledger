(ns finance.data.book
  "Books are top-level containers of financial data. Each book contains a set
  of accounts, local commodity and price data, and a transaction history."
  (:require
    [clojure.spec.alpha :as s]
    [finance.data.core :as data :refer [defattr defentity]]))


;; ## Data Attributes

(defattr ::id
  "Unique short slug identifying the book."
  ::data/some-string
  :db/unique :db.unique/identity)


(defattr ::title
  "Human-meaningful name for the the book."
  ::data/some-string)


(defattr ::description
  "Longer-form text describing the book."
  string?)


;; ## Normal Form

(defentity :finance.data/book
  :req [::id]
  :opt [::title
        ::description])


;; ## Tree Form

(s/def ::accounts
  (s/coll-of (data/tree-spec :finance.data/account)
             :kind vector?))


(s/def ::journal
  (s/coll-of (data/tree-spec :finance.data/transaction)
             :kind vector?))


(defmethod data/tree-form :finance.data/book
  [_]
  (s/keys :req [::id
                ::title]
          :opt [::description
                ::accounts
                ::journal
                ;; local commodities
                ,,,]))


(defmethod data/normalize-tree :finance.data/book
  [ctx book]
  (let [ctx (assoc ctx ::id (::id book))
        accounts (into []
                       (mapcat (partial data/normalize-tree ctx))
                       (::accounts book))
        ctx (assoc ctx ::accounts accounts)]
    (concat
      [(dissoc book
               ::accounts
               ::journal)]
      accounts
      (mapcat
        (partial data/normalize-tree ctx)
        (::journal book)))))
