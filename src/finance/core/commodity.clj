(ns finance.core.commodity
  "Functions dealing with financial commodities and prices."
  (:require
    [datascript.core :as d]))


; Static checks:
; - schema validates

; Historical checks:
; - check for duplicate prices?


(defn find-duplicate-prices
  [db]
  (d/q '[:find ?p1 ?p2
         :where [(< ?p1 ?p2)]
                [?p1 :finance.price/commodity ?c]
                [?p2 :finance.price/commodity ?c]
                [?p1 :time/at ?time]
                [?p2 :time/at ?time]]
       db))
