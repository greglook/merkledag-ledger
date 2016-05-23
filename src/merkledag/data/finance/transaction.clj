(ns merkledag.data.finance.transaction
  "Functions dealing with financial transactions."
  (:require
    [datascript.core :as d]
    (merkledag.data.finance
      [entry :as entry]
      [types :as types])))


; Static checks:
; - must be at least one entry
; - all entry accounts belong to the same books
; - real posting weights must sum to zero

; Historical checks:
; ...


(defn tx-balance
  "Calculates the sum of the posting weights for the given entries. Virtual
  postings are not included. Returns a map of commodity codes to balances."
  [entries]
  (reduce
    (fn [balances entry]
      (if-let [weight (entry/entry-weight entry)]
        (update balances (:commodity weight) (fnil + 0M) (:value weight))
        balances))
    (sorted-map)
    entries))


(defn interpolate-entries
  "Fills in any missing transaction data by interpolating other entries."
  [entries]
  (let [missing-amounts (->> entries
                             (filter (comp #{:finance.entry/posting} :data/type))
                             (filter (complement :finance.posting/amount)))]
    (cond
      (zero? (count missing-amounts))
        entries

      (= 1 (count missing-amounts))
        (let [balances (tx-balance entries)
              [before [missing & after]]
              (split-with #(or (not= :finance.entry/posting (:data/type %))
                               (:finance.posting/amount %))
                          entries)]
          (when (< 1 (count balances))
            (throw (ex-info (str "Cannot infer missing posting amount when multiple commodities are in use: " (keys balances))
                            {:entries entries
                             :balances balances})))
          (concat
            before
            [(assoc missing :finance.posting/amount (types/->Quantity (- (val (first balances))) (key (first balances))))]
            after))

      :else
        (throw (ex-info "Cannot infer posting values when more than one is missing an amount"
                        {:entries entries})))))
