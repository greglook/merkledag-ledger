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
  [posting opts]
  (let [balance (:finance.balance/amount posting)
        amount (:finance.posting/amount posting)
        weight (:finance.posting/weight posting)
        price (:finance.posting/price posting)
        cost (:finance.posting/cost posting)
        scale (or price (:amount cost))]
    (->>
      [; Balance check should match commodity in amount.
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
       ; TODO: Total of items in invoice should match amount.
       ]
      (remove nil?)
      (seq))))


(defn check-entry
  "Returns a sequence of maps describing errors with the entry, if any."
  [entry opts]
  (case (:data/type entry)
    :finance.entry/posting
      (check-posting entry opts)
    :finance.entry/balance-check
      [(when-not (:finance.balance/amount entry)
         {:error :missing-balance-amount
          :message "balance-check entries must have a balance amount"
          :entry entry})]
    (:finance.entry/note
     :finance.entry/open-account
     :finance.entry/close-account)
      nil
    [{:error :bad-entry-type
      :message "invalid entry data type"
      :type (:data/type entry)}]))


(defn weight
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


(defn balance
  "Calculates the sum of the posting weights for the given entries. Virtual
  postings are not included. Returns a map of commodity codes to balances."
  [entries]
  (reduce
    (fn [balances entry]
      (if-let [weight (weight entry)]
        (update balances (:commodity weight) (fnil + 0M) (:value weight))
        balances))
    (sorted-map)
    entries))


(defn interpolate-amounts
  "Fills in any missing posting amounts by interpolating other entries."
  [entries]
  (let [amount-missing? #(and (= :finance.entry/posting (:data/type %))
                              (nil? (:finance.posting/amount %)))
        missing (count (filter amount-missing? entries))]
    (cond
      (zero? missing)
        entries

      (= 1 missing)
        (let [balances (balance entries)]
          (when (empty? balances)
            (throw (ex-info "Cannot infer missing posting amount with no real postings"
                            {:entries entries})))
          (when (< 1 (count balances))
            (throw (ex-info (str "Cannot infer missing posting amount when multiple commodities are in use: " (keys balances))
                            {:entries entries
                             :balances balances})))
          (let [[before [blank & after]] (split-with (complement amount-missing?) entries)
                counter-weight (types/q (- (val (first balances))) (key (first balances)))]
            (concat
              before
              [(assoc blank :finance.posting/amount counter-weight)]
              after)))

      :else
        (throw (ex-info "Cannot infer posting values when more than one is missing an amount"
                        {:entries entries})))))
