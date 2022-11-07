(ns finance.data.entry
  "Core data definitions and functions for individual entries in an account."
  (:require
    [clojure.spec.alpha :as s]
    [finance.data.account :as account]
    [finance.data.core :as data :refer [defattr defident defref defentity]]
    [finance.data.time :as time]
    [finance.data.transaction :as transaction]))


;; ## Data Attributes

(defref ::account
  "Account the entry belongs to."
  ::account/id)


(defref ::transaction
  "Transaction this entry is part of."
  ::transaction/id)


(defident ::id
  "Unique identifier for the entry."
  "ent")


(defattr ::date
  "Calendar date the entry occurred at. Used to order entries inside the
  account journal, but does not override the transaction date."
  ::time/local-date)


(defattr ::time
  "Time the entry occurred in the real world. Used to order entries inside
  the account journal, but does not override the transaction time."
  ::time/instant)


(defattr ::interval
  "Interval of time over which the entry applies."
  ::time/interval)


(defattr ::txn-rank
  "Extra numeric value to determine the ordering of entries within a
  transaction."
  number?)


(defattr ::journal-rank
  "Extra numeric value to determine the ordering of entries within an account
  register which have the same timestamp."
  number?)


(defattr ::external-id
  "String containing an external identifier for the entry, for deduplication."
  ::data/some-string
  :db/index true)


(defattr ::description
  "Extra human-readable text description for an entry."
  string?)


(defattr ::source-lines
  "Set of lines pulled from third-party sources that this entry represents."
  (s/coll-of string? :kind set?)
  :db/cardinality :db.cardinality/many)


;; ## Entry Entities

(defmulti entry-spec ::data/type)


(defmethod data/normal-form :finance.data/entry
  (s/merge
    (s/keys :req [::account/id
                  ::transaction/id
                  ::id
                  ::date]
            :opt [::time
                  ::rank
                  ::description
                  ::external-id
                  ::source-lines])
    (s/multi-spec entry-spec ::data/type)))


#_
(defmacro defentry
  "Define a new type of financial entry. Takes a simple keyword like `:posting`
  and defines a new entity in the entry namespace like
  `:finance.data.entry/posting`. Adds common entry fields automatically."
  [type-name doc-str & {:as spec-keys}]
  (let [type-key (keyword "finance.data.entry" (name type-name))
        auto-req [::date]
        auto-opt [::time
                  ::rank
                  ::description
                  ::external-id
                  ::source-lines]
        req-keys (->> (:req spec-keys)
                      (concat auto-req)
                      (distinct)
                      (vec))
        opt-keys (->> (:opt spec-keys)
                      (concat auto-opt)
                      (remove (set req-keys))
                      (distinct)
                      (vec))]
    `(do
       (defentity ~type-key
         ~doc-str
         :req ~req-keys
         :opt ~opt-keys)
       (defmethod journal-entry ~type-key
         [~'_]
         ~type-key))))


;; ## Basic Entry Types

#_
(defentry :open-account
  "Marks an account as being open for transactions.")


#_
(defentry :close-account
  "Marks an account as being closed to new transactions.")


#_
(defentry :note
  "Free-form note in the account history."
  :req [::description]
  :opt [::interval])


;; ## Functions

; Historical checks:
; - lot-id should specify a real previous posting
; - lot-cost and lot-date should match identified posting
; - balance amount should match current account value for that commodity
;   (within tolerance)
; - commodity in amount must be allowed by account

#_
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


#_
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


#_
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


#_
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


#_
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
