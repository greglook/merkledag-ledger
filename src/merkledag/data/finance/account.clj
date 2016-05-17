(ns merkledag.data.finance.account
  "Functions dealing with financial accounts."
  (:require
    [datascript.core :as d]))


; Static checks:
; - schema validates

; Book-level checks:
; - no account path should be a prefix of another account
; - warn about accounts with no entries

; Historical checks:
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


; TODO: get an account's register
