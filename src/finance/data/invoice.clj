(ns finance.data.invoice
  "An invoice is an itemized list of charges, with information about each item
  such as a description, quantity, cost, etc."
  (:require
    [clojure.spec.alpha :as s]
    [finance.data.book :as book]
    [finance.data.core :as data :refer [defattr defentity defref]]))


;; ## Data Attributes

(defref ::book
  "Book the invoice belongs to."
  ::book/id)


(defident ::id
  "Unique identifier for an invoice."
  "inv")


;; TODO: other invoice attributes; should be able to independently model a receipt


;; ## Normal Form

(defentity :finance.data/invoice
  :req [::book/id
        ::id
        ,,,]
  :opt [,,,])


;; ## Tree Form

;; TODO: these show up on postings, though...
(s/def ::items
  (s/coll-of (data/tree-spec :finance.data/item)
             :kind vector?))


(defmethod data/tree-form :finance.data/invoice
  [_]
  (s/keys :req [,,,]
          :opt [,,,]))
