(ns merkledag.data.finance.transaction
  "Functions dealing with financial transactions."
  (:require
    [datascript.core :as d]
    [merkledag.data.finance.types :as types]))


; Static checks:
; - must be at least one entry
; - all entry accounts belong to the same books
; - real posting weights must sum to zero

; Historical checks:
; ...


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
        (let [[balance commodities]
              (reduce (fn [[total cs] entry]
                        (if-let [amount (:finance.posting/amount entry)]
                          [(+ total (:value amount)) (conj cs (:commodity amount))]
                          [total cs]))
                      [0M #{}]
                      entries)
              [before [missing & after]]
              (split-with #(or (not= :finance.entry/posting (:data/type %))
                               (:finance.posting/amount %))
                          entries)]
          (when (not= :finance.entry/posting (:data/type missing))
            (throw (ex-info (str "Cannot infer missing amount for non-posting entry: " (:data/type missing))
                            (:entries entries
                             :missing missing))))
          (when (< 1 (count commodities))
            (throw (ex-info (str "Cannot infer missing posting amount when multiple commodities are in use: " commodities)
                            {:entries entries
                             :commodities commodities})))
          (concat
            before
            [(assoc missing :finance.posting/amount (types/->Quantity (- balance) (first commodities)))]
            after))

      :else
        (throw (ex-info "Cannot infer posting values when more than one is missing an amount"
                        {:entries entries})))))
