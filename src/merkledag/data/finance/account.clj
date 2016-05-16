(ns merkledag.data.finance.account
  "Functions dealing with financial accounts."
  (:require
    [datascript.core :as d]))


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



; TODO: validate an account definition
; TODO: get an account's register
