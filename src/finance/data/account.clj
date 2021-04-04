(ns finance.data.account
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [finance.data.book :as book]
    [finance.data.commodity :as commodity]
    [finance.data.core :as data :refer [defattr defentity]]))


(def account-types
  "Set of names for some common account types."
  #{:cash
    :savings
    :checking
    :demand-deposit
    :certificate-of-deposit
    :brokerage
    :traditional-401k
    :roth-401k
    :traditional-ira
    :roth-ira
    :health-savings
    :credit-card
    :mortgage
    :student-loan
    :loan
    :reward-program
    :bitcoin
    :property})


(defattr ::id
  "Link to the account's root node."
  ::data/some-string
  :db/unique :db.unique/identity)


(defattr ::path
  "Path segments to uniquely identify the account within a book."
  (s/coll-of ::data/some-string
             :kind vector?
             :min-elements 1)
  :db/index true)


(defattr ::alias
  "Keyword alias to refer to the account by."
  simple-keyword?
  :db/index true)


(defattr ::title
  "Human-readable title to display for the account."
  ::data/some-string)


(defattr ::description
  "Additional descriptive text."
  ::data/some-string)


(defattr ::type
  "Keyword identifying the type of account."
  ;; TODO: open this up to user extension later
  account-types)


(defattr ::external-id
  "String giving the account's external identifier, such as an account number."
  ::data/some-string
  :db/unique :db.unique/identity)


(defattr ::commodities
  "Set of commodities which are valid for the account to contain."
  (s/coll-of ::commodity/code :kind set?)
  :db/cardinality :db.cardinality/many)


(defattr ::links
  "String identifiers linking related accounts together."
  ::data/some-string
  :db/cardinality :db.cardinality/many
  :db/index true)


(defentity :finance.data/account
  "..."
  :req [::book/id
        ::id
        ::path]
  :opt [::title
        ::description
        ::alias
        ::type
        ::external-id
        ::commodities
        ::links])
