(ns finance.data.balance
  "Balance checks provide a mechanism for asserting the state of an account at
  specific points in time. This is a tool for tying the data to known-good
  numbers verified in the real world."
  (:require
    [finance.data.core :as data :refer [defattr]]
    [finance.data.entry :as entry]
    [finance.data.quantity :as quantity]))


;; ## Data Attributes

(defattr ::amount
  "Amount of a certain commodity the account should contain."
  ::quantity/q)


(derive ::entry/balance-check :finance.data/entry)


;; ## Normal Form

(defmethod data/normal-form ::entry/balance-check
  (s/merge
    (s/keys :req [::entry/id
                  ::account/id
                  ::transaction/id
                  ::entry/date]
            :opt [::entry/time
                  ::entry/rank
                  ::entry/description
                  ::entry/external-id
                  ::entry/source-lines])
    (s/keys :req [::amount])))


;; ## Tree Form

(defmethod data/tree-spec ::entry/balance-check
  [_]
  (s/merge
    (s/keys :req [::entry/account-ref]
            :opt [::entry/id
                  ::entry/date
                  ::entry/time
                  ::entry/txn-rank
                  ::entry/journal-rank
                  ::entry/external-id
                  ::entry/description
                  ::entry/source-lines])
    (s/keys :req [::amount])))


(defmethod data/normalize-tree ::entry/balance-check
  [ctx entry]
  (let [accounts (:finance.data.book/accounts ctx)
        account-ref (::entry/account-ref entry)
        account (or (account/find-account accounts account-ref)
                    (throw (ex-info (str "Could not find account definition for reference "
                                         account-ref)
                                    {:accounts accounts
                                     :account-ref account-ref})))]
    [(-> entry
         ;; TODO: adopt transaction date?
         (assoc (or (::entry/id entry) (entry/gen-id))
                ::account/id (::account/id account)
                ::transaction/id (::transaction/id ctx))
         (dissoc ::entry/account-ref))]))
