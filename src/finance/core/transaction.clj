(ns finance.core.transaction
  "Functions dealing with financial transactions."
  (:require
    [datascript.core :as d]
    (finance.core
      [entry :as entry]
      [types :as types])))


; Static checks:
; - must be at least one entry
; - all entry accounts belong to the same books
; - real posting weights must sum to zero

; Historical checks:
; ...


(defn select-transactions
  [db book & query-args]
  (let [query (if (and (= 1 (count query-args))
                       (map? query-args))
                query-args
                (apply hash-map query-args))]
    (->>
      ; TODO: figure out what query args are suitable
      ; minimally, filtering on date should work
      (d/q '{:find [?tx ?date (min ?time)]
             :in [$ ?book]
             :where [[?a :finance.account/book ?book]
                     [?e :finance.entry/account ?a]
                     [?e :time/at ?time]
                     [?tx :finance.transaction/entries ?e]
                     [?tx :finance.transaction/date ?date]]}
           db book)
      (sort-by (comp vec rest))
      (map (comp (partial d/entity db) first)))))
