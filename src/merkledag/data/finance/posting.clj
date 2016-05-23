(ns merkledag.data.finance.posting
  "Functions dealing with postings to financial accounts."
  (:require
    [datascript.core :as d]
    [merkledag.data.finance.types :as types]))


; Static checks:
; - schema validates
; - amount and price must have different commodities
; - weight only makes sense when price is specified
; - weight must be in same commodity as price
; - weight must be within tolerance of amount * price
; - balance check should match commodity in amount
; - total of items in invoice must match amount
; - commodity in amount must be allowed by account

; Historical checks:
; - lot-id should specify a real previous posting
; - lot-cost and lot-date should match identified posting
; - balance amount should match current account value for that commodity
;   (within tolerance)


(defn entry-weight
  "Calculates the balancing weight for a posting. This handles explicit
  weights, costs, and price conversions. Returns nil if the entry is not a
  concrete posting."
  [posting]
  (let [amount (:finance.posting/amount posting)]
    (when (and (= :finance.entry/posting (:data/type posting))
               (not (:finance.posting/virtual posting))
               amount)
      (or
        ; Explicit weight.
        (:finance.posting/weight posting)

        ; Price conversion.
        (when-let [price (:finance.posting/price posting)]
          (types/q (* (:value amount) (:value price)) (:commodity price)))

        ; Cost conversion.
        (when-let [cost (:finance.posting/cost posting)]
          (types/q (* (:value amount) (:value (:amount cost))) (:commodity (:amount cost))))

        ; Fall back to amount.
        amount))))
