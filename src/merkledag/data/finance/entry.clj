(ns merkledag.data.finance.entry
  "Functions dealing with postings to financial accounts."
  (:require
    [datascript.core :as d]
    [merkledag.data.finance.types :as types]))


; Historical checks:
; - lot-id should specify a real previous posting
; - lot-cost and lot-date should match identified posting
; - balance amount should match current account value for that commodity
;   (within tolerance)
; - commodity in amount must be allowed by account


(defn check-posting
  "Returns a sequence of maps describing errors with the posting, if any."
  [posting]
  (let [balance (:finance.balance/amount posting)
        amount (:finance.posting/amount posting)
        weight (:finance.posting/weight posting)
        price (:finance.posting/price posting)
        cost (:finance.posting/cost posting)
        scale (or price (:amount cost))]
    (->>
      [; TODO: validate schema
       ; Balance check should match commodity in amount.
       (when (and amount balance (not= (:commodity amount) (:commodity balance)))
         {:error :bad-balance-commodity
          :message "balance check commodity must match amount"
          :amount amount
          :balance balance})
       ; Price must be in a different commodity than amount.
       (when (and amount scale (= (:commodity amount) (:commodity price)))
         {:error :recursive-price
          :message "posting amount must be priced in a different commodity"
          :commodity (:commodity amount)})
       ; Cost must be in a different commodity than amount.
       (when (and amount cost (= (:commodity amount) (:commodity cost)))
         {:error :recursive-cost
          :message "posting amount must record cost in a different commodity"
          :commodity (:commodity amount)})
       ; Weight only makes sense when a scale is specified.
       (when (and weight (nil? scale))
         {:error :redundant-weight
          :message "posting weight without conversion is redundant"
          :weight weight})
       ; Weight must be in same commodity as scale.
       (when (and weight scale (not= (:commodity weight) (:commodity scale)))
         {:error :bad-weight-commodity
          :message "posting weight must match scale commodity"
          :weight weight
          :scale scale})
       ; TODO: Weight must be within tolerance of amount * price.
       ; TODO: Total of items in invoice must match amount.
       ]
      (remove nil?)
      (seq))))


(defn entry-weight
  "Calculates the balancing weight for a transaction entry. This handles
  explicit weights, costs, and price conversions. Returns a weight quantity, or
  nil if the entry is not a concrete posting."
  [entry]
  (let [amount (:finance.posting/amount entry)]
    (when (and (= :finance.entry/posting (:data/type entry))
               (not (:finance.posting/virtual entry))
               amount)
      (or
        ; Explicit weight.
        (:finance.posting/weight entry)

        ; Price conversion.
        (when-let [price (:finance.posting/price entry)]
          (types/q (* (:value amount) (:value price)) (:commodity price)))

        ; Cost conversion.
        (when-let [cost (:finance.posting/cost entry)]
          (types/q (* (:value amount) (:value (:amount cost))) (:commodity (:amount cost))))

        ; Fall back to amount.
        amount))))
