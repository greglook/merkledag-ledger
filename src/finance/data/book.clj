(ns finance.data.book
  "Books are top-level containers of financial data. Each book contains a set
  of accounts, local commodity and price data, and a transaction history."
  (:require
    [finance.data.core :as data :refer [defattr defentity]]))


(defattr ::id
  "Unique short slug identifying the book."
  ::data/some-string)


(defattr ::title
  "Human-meaningful name for the the book."
  ::data/some-string)


(defattr ::description
  "Longer-form text describing the book."
  string?)


(defentity :finance.data/book
  "Book data entity."
  :req [::id
        ::title]
  :opt [::description])
