(ns finance.data.account
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [datascript.core :as ds]
    [finance.data.book :as book]
    [finance.data.commodity :as commodity]
    [finance.data.core :as data :refer [defattr defentity]]))


;; ## Data Specs

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



;; ## Functions

; Static checks:
; - schema validates

; Historical checks:
; - warn about accounts with no entries?
; - first entry must be an open-account entry
; - there must be AT MOST one open-account entry
; - if closed, last entry must be a close-account entry
; - there must be AT MOST one close-account entry
; - account should only ever contain allowed commodities (move to posting?)


(defn find-account
  "Returns the entity for an account identified by a keyword alias or a path
  vector in the given set of books."
  [db book account-ref]
  (let [attr-key (if (keyword? account-ref)
                   ::alias
                   ::path)
        query {:find '[[?a]]
               :in '[$ ?book ?id]
               :where [['?a ::book/id '?book]
                       ['?a attr-key '?id]]}
        [account-id] (ds/q query db book account-ref)]
    (when account-id
      (ds/entity db account-id))))


#_
(defn open-entry
  "Returns the entry opening the given account."
  [db account]
  (when-let [account (if (number? account) account (:db/id account))]
    (->>
      (d/q '[:find [?e]
             :in $ ?a
             :where [?e :data/type :finance.entry/open-account]
                    [?e :finance.entry/account ?a]]
           db account)
      (first)
      (d/entity db))))


#_
(defn get-register
  "Returns a sequence of ordered entry entities for the given account. The
  `account` arg may be an entity or id."
  [db account]
  ; TODO: time constraints
  (when-let [account (if (number? account) account (:db/id account))]
    (->>
      (d/q '[:find ?e ?time ?rank
             :in $ ?a
             :where [?e :finance.entry/account ?a]
                    [?e :finance.entry/rank ?rank]
                    [?e :time/at ?time]]
           db account)
      (sort-by (comp vec rest))
      (map (comp (partial d/entity db) first)))))
