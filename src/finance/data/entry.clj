(ns finance.data.entry
  "Core data definitions and functions for individual entries in an account."
  (:require
    [clojure.spec.alpha :as s]
    [finance.data.account :as account]
    [finance.data.core :as data :refer [defattr defentity]]
    [finance.data.time :as time]))


;; ## General Attributes

#_
(defattr ::account
  "Name of the account the entry is related to."
  ;; TODO: is this flexibility desirable outside a textual form?
  (s/or :path ::account/path
        :alias ::account/alias)
  :db/valueType :db.type/ref)


(defattr ::date
  "Calendar date the entry occurred at. Used to order entries inside the
  account journal, but does not override the transaction date."
  ::time/local-date)


(defattr ::time
  "Time the entry occurred in the real world. Used to order entries inside
  the account journal, but does not override the transaction time."
  ::time/instant)


(defattr ::interval
  "Interval of time over which the entry applies."
  ::time/interval)


(defattr ::rank
  "Extra numeric value to determine the ordering of entries within an account
  register which have the same timestamp."
  number?)


(defattr ::external-id
  "String containing an external identifier for the entry, for deduplication."
  ::data/some-string
  :db/index true)


(defattr ::source-lines
  "Set of lines pulled from third-party sources that this entry represents."
  ;; TODO: how does this collection spec + multi-cardinality work? Should this just be `string?`?
  (s/coll-of string? :kind set?)
  :db/cardinality :db.cardinality/many)



;; ## Entry Entities

(defmulti journal-entry ::data/type)


(s/def :finance.data/entry
  (s/multi-spec journal-entry ::data/type))


(defmacro defentry
  "Define a new type of financial entry. Takes a simple keyword like `:posting`
  and defines a new entity in the entry namespace like
  `:finance.data.entry/posting`. Adds common entry fields automatically."
  [type-name doc-str & {:as spec-keys}]
  (let [type-key (keyword "finance.data.entry" (name type-name))
        auto-req [::account/id
                  ::date]
        auto-opt [::time
                  ::rank
                  ::description
                  ::external-id
                  ::source-lines]
        req-keys (->> (:req spec-keys)
                      (concat auto-req)
                      (distinct)
                      (vec))
        opt-keys (->> (:opt spec-keys)
                      (concat auto-opt)
                      (remove (set req-keys))
                      (distinct)
                      (vec))]
    `(do
       (defentity
         ~type-key
         ~doc-str
         :req ~req-keys
         :opt ~opt-keys)
       (defmethod journal-entry ~type-key
         [_]
         ~type-key))))



;; ## Basic Entry Types

(defentry :open-account
  "Marks an account as being open for transactions.")


(defentry :close-account
  "Marks an account as being closed to new transactions.")


(defentry :note
  "Free-form note in the account history."
  :req [::description]
  :opt [::interval])
