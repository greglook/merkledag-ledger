(ns finance.core.account
  "Functions dealing with financial accounts."
  (:require
    [datascript.core :as d]))


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
                   :finance.account/alias
                   :finance.account/path)
        query {:find '[[?a]]
               :in '[$ ?book ?id]
               :where [['?a :finance.account/book '?book]
                       ['?a attr-key '?id]]}
        [account-id] (d/q query db book account-ref)]
    (when account-id
      (d/entity db account-id))))


(defn find-account!
  "Finds an account as in `find-account`, but throws an exception if the
  account cannot be located."
  [db book account-ref]
  (or (find-account db book account-ref)
      (throw (ex-info (str "No account found matching id: " (pr-str account-ref))
                      {:error :not-found, :account account-ref}))))


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
