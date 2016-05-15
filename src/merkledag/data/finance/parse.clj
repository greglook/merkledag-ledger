(ns merkledag.data.finance.parse
  "Ledger file parsing code."
  (:require
    (clj-time
      [coerce :as ctime]
      [core :as time]
      [format :as ftime])
    [clojure.java.io :as io]
    [clojure.string :as str]
    [instaparse.core :as parse]
    [merkledag.data.finance.types :as types]))


(def ledger-parser
  (parse/parser (io/resource "grammar/ledger.bnf")))


(def time-format
  "Formats to accept for time values. This parses dates in the **local**
  time-zone if one is not specified."
  (ftime/formatter
    (time/default-time-zone)
    "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
    "yyyy-MM-dd'T'HH:mm:ss.SSS"
    "yyyy-MM-dd'T'HH:mm:ssZ"
    "yyyy-MM-dd'T'HH:mm:ss"
    "yyyy-MM-dd'T'HH:mmZ"
    "yyyy-MM-dd'T'HH:mm"))



;; ## Parse Helpers

(defn- parse-time
  [date time zone]
  (ftime/parse
    (if zone
      (ftime/with-zone time-format zone)
      time-format)
    (str date "T" time))
  #_
  (time/to-time-zone
    (ftime/parse
      (if zone
        (ftime/with-zone time-format zone)
        time-format)
      (str date "T" time))
    time/utc))


(defn- update-time
  [obj time-key date-key]
  (let [date (get obj date-key)
        time (get obj time-key)]
    (assoc obj
      time-key
      (if (and (vector? time) (= :Time (first time)))
        (let [[_ time-str zone] time]
          (parse-time date time-str zone))
        (or time (ctime/to-date-time date))))))


(defn- join-field
  [obj field delimiter]
  (let [v (get obj field)]
    (if (sequential? v)
      (assoc obj field (str/join delimiter v))
      obj)))


(defn- lift-meta
  ([obj meta-tag field-key]
   (lift-meta obj meta-tag field-key identity))
  ([obj meta-tag field-key f]
   (if-let [value (get-in obj [:data/tags meta-tag])]
     (-> obj
         (assoc field-key (f value))
         (update :data/tags dissoc meta-tag)
         (as-> obj'
           (if (empty? (:data/tags obj'))
             (dissoc obj' :data/tags)
             obj')))
     obj)))


(defn- assoc-some
  ([x k v]
   (if (some? v)
     (assoc x k v)
     x))
  ([x k v & more]
   (->> more
        (partition-all 2)
        (cons [k v])
        (reduce #(assoc-some %1 (first %2) (second %2)) x))))


(defn- collect-one
  [k]
  (fn [children]
    (when-let [[match :as matches] (seq (filter #(and (vector? %)
                                                      (= k (first %)))
                                                children))]
      (when (< 1 (count matches))
        (throw (ex-info
                 (str "Multiple values present for (collect-one "
                      (pr-str k) ")")
                 {:key k, :matches matches})))
      (when (< 2 (count match))
        (throw (ex-info
                 (str "Cannot unbox child " (pr-str k) " which has "
                      (dec (count match)) " children")
                 {:key k, :match match})))
      (second match))))


(defn- collect-all
  [k]
  (fn [children]
    (when-let [matches (seq (filter #(and (vector? %) (= k (first %)))
                                    children))]
      (when-let [bad-children (seq (filter #(< 2 (count %)) matches))]
        (throw (ex-info
                 (str "Cannot unbox " (count bad-children) " " (pr-str k)
                      " children which have too many entries")
                 {:key k, :bad-children bad-children})))
      (map second matches))))


(defn- collect-set
  [k]
  (comp not-empty set (collect-all k)))


(defn- collect-map
  [k]
  (fn [children]
    (when-let [matches (seq (filter #(and (vector? %)
                                          (= k (first %)))
                                    children))]
      (when-let [bad-children (seq (filter #(not= 3 (count %)) matches))]
        (throw (ex-info
                 (str "Cannot unbox " (count bad-children) " " (pr-str k)
                      " children which have the wrong entry counts")
                 {:key k, :bad-children bad-children})))
      (into {} (map (comp vec rest) matches)))))


(defn- collect
  [initial collectors children]
  (reduce-kv
    (fn [data k collector]
      (if-let [v (collector children)]
        (assoc data k v)
        data))
    initial
    collectors))



;; ## Parse Interpreters

(def basic-transforms
  {:Date ftime/parse-local-date

   :DateTime
   (fn ->date-time
     [date [_ time tz]]
     (parse-time date time tz))

   :TimeZone
   (fn ->time-zone
     [zone]
     (case zone
       "Z" time/utc
       (time/time-zone-for-id zone)))

   :Number
   (fn ->number
     [& digits]
     (BigDecimal. (str/join digits)))

   :Percentage
   (fn ->percentage
     [number]
     (/ number 100))

   :Quantity
   (fn ->quantity
     [& [v1 v2 :as children]]
     (cond
       (and (= "0" v1) (nil? v2))
         nil
       (and (number? v1) (symbol? v2))
         (types/->Quantity v1 v2)
       (and (symbol? v1) (number? v2))
         (types/->Quantity v2 v1)
       :else
         (throw (ex-info (str "Unknown quantity format! " (pr-str [v1 v2]))
                         {:form children}))))})


(def commodity-transforms
  {:CommodityCode
   (fn ->commodity-code
     [code]
     (if (= "$" code) 'USD (symbol code)))

   :CommodityDefinition
   (fn ->commodity
     [code & children]
     (collect
       {:data/type :finance/commodity
        :finance.commodity/code code}
       {:title    (collect-one :CommodityNote)
        ::format  (collect-one :CommodityFormat)
        ::options (collect-all :CommodityOption)}
       children))

   :CommodityPrice
   (fn ->commodity-price
     [date code price]
     {:data/type :finance/price
      :time/at date
      :finance.price/commodity code
      :finance.price/value price})})


(def account-transforms
  {:AccountPathSegment (comp str/join list)
   :AccountPath vector
   :AccountAlias keyword

   :AccountDefinition
   (fn ->account-definition
     [path & children]
     (let [data (collect
                  {:data/type :finance/account
                   :title (last path)
                   :finance.account/path path}
                  {:finance.account/alias (collect-one :AccountAliasDirective)
                   ::assertion            (collect-one :AccountAssertion)
                   :description           (collect-one :AccountNote)}
                  children)
           assertion (::assertion data)]
       (dissoc
         (if-let [match (and assertion (re-find #"commodity == \"(\S+)\""
                                                assertion))]
           (assoc data :finance.account/allowed-commodities
                  #{((commodity-transforms :CommodityCode) (second match))})
           data)
         ::assertion)))})


(def metadata-transforms
  {:TagName keyword
   :MetaEntry
   (fn ->meta-entry
     ([k]   [:MetaEntry k true])
     ([k v] [:MetaEntry k v]))

   :SourceMeta
   (fn ->source-meta
     [src line]
     [:SourceMeta {:source src, :line line}])})


(def transaction-transforms
  {:Transaction
   (fn ->transaction
     [date & children]
     (-> {:data/type :finance/transaction
          :finance.transaction/date date
          ; TODO: should take into account timezone
          :time/at (ctime/to-date-time date)}
         (collect
           {:title                       (collect-one :TxMemo)
            :description                 (collect-all :MetaComment)
            :time/at                     (collect-one :TimeMeta)
            :finance.transaction/flag    (collect-one :TxFlag)
            :finance.transaction/code    (collect-one :TxCode)
            :finance.transaction/entries (collect-all :Posting)
            :data/tags                   (collect-map :MetaEntry)}
           children)
         (join-field :description "\n")
         (update :finance.transaction/entries vec)
         (update-time :time/at :finance.transaction/date)
         (lift-meta :UUID :data/ident (partial str "finance:transaction:"))
         (lift-meta :link :finance.transaction/links hash-set)
         (as-> tx
           (let [tx-time (:time/at tx)]
             (-> tx
                 (update :finance.transaction/entries
                         (partial mapv #(if (:time/at %) % (assoc % :time/at tx-time))))
                 (dissoc :time/at)))
           (assoc tx :finance.transaction/entries
                  (mapv #(assoc %1 :finance.entry/rank %2)
                        (:finance.transaction/entries tx)
                        (range))))))

   :TxFlag
   (fn ->posting-status
     [chr]
     [:TxFlag (case chr "!" :pending, "*" :cleared, :uncleared)])

   :Posting
   (fn ->posting
     [account & children]
     (let [posting-type (case (first account)
                          :RealAccountRef :real
                          :VirtualAccountRef :virtual
                          :BalancedVirtualAccountRef :balanced-virtual)
           [amount children] (if (vector? (first children))
                               [nil children]
                               [(first children) (rest children)])]
       [:Posting
        (->
          {:data/type :finance.entry/posting
           :finance.entry/account (second account)}
          (assoc-some
            :finance.posting/amount amount)
          (collect
            {:finance.posting/cost     (collect-one :PostingLotCost)
             :finance.posting/lot-date (collect-one :PostingLotDate)
             :finance.posting/price    (collect-one :PostingPrice)
             :finance.balance/amount   (collect-one :PostingBalance)
             ::date                    (collect-one :PostingDate)
             :time/at                  (collect-one :TimeMeta)
             :data/sources             (collect-set :SourceMeta)
             :data/tags                (collect-map :MetaEntry)
             :description              (collect-all :MetaComment)
             :finance.posting/invoice  (collect-all :LineItem)}
            children)
          (update-time :time/at ::date)
          (dissoc ::date)
          (join-field :description "\n")
          (lift-meta :Payee :finance.posting/payee)
          (as-> posting
            (cond-> posting
              (seq (:finance.posting/invoice posting))
                (assoc :finance.posting/invoice
                       {:data/type :finance/invoice
                        :finance.invoice/items (mapv #(assoc %1 :finance.item/rank %2)
                                                     (:finance.posting/invoice posting)
                                                     (range))})
              (= posting-type :virtual)
                (assoc :finance.posting/virtual true)
              (and (or (nil? (first (:form amount)))
                       (zero? (first (:form amount))))
                   (= :balanced-virtual posting-type)
                   (:finance.balance/amount posting))
                (-> (assoc :data/type :finance.entry/balance-check)
                    (dissoc :finance.posting/amount)))))]))

   :LineItemTaxGroup keyword
   :LineItemTaxGroups
   (fn ->tax-groups
     [& groups]
     [:LineItemTaxGroups (set groups)])

   :LineItem
   (fn ->line-item
     [desc & children]
     [:LineItem
      (collect
        {:title desc
         :data/type :finance/item}
        {:finance.item/total       (collect-one :LineItemTotal)
         :finance.item/amount      (collect-one :LineItemAmount)
         :finance.item/price       (collect-one :LineItemPrice)
         :finance.item/tax-groups  (collect-one :LineItemTaxGroups)
         :finance.item/tax-applied (collect-one :LineItemTaxApplied)}
        children)])})


(def ledger-transforms
  (merge basic-transforms
         commodity-transforms
         account-transforms
         metadata-transforms
         transaction-transforms))


(defn interpret-parse
  [tree]
  (try
    (parse/transform ledger-transforms tree)
    (catch Exception e
      (throw (ex-info (str "Failed to interpret parse tree: " tree)
                      {:tree tree}
                      e)))))



;; ## File Parsing

(defn- add-source
  "Adds a `:data/sources` key to associative values in a parsed entry."
  [source entry]
  (if (map? entry)
    (update entry :data/sources (fnil conj #{}) (apply subs source (parse/span entry)))
    entry))


(defn- check-parse!
  "Returns the value if it is not the result of a parsing failure. Throws an
  exception on failure values."
  [value]
  (when (parse/failure? value)
    (throw (ex-info (str "Parsing entry failed: " value)
                    {:failure (parse/get-failure value)})))
  value)


(defn group-lines
  "Takes a sequence of lines and returns a new sequence of groups of lines
  which were separated by blank lines in the input."
  [lines]
  (->> lines
       (partition-by #{""})
       (remove #(every? #{""} %))
       (map #(str (str/join "\n" %) "\n"))))


(defn parse-group
  "Parses a group of lines from a file, returning an interpreted ledger entry.
  Throws an exception if the parser fails."
  [group]
  (->> group
       (ledger-parser)
       (check-parse!)
       (interpret-parse)
       (map (partial add-source group))))


(defn parse-file
  "Parse a single file, returning a sequence of interpreted ledger entries."
  [file & {:keys [group-fn], :or {group-fn group-lines}}]
  (->> file
       (io/file)
       (io/reader)
       (line-seq)
       (group-fn)
       (mapcat parse-group)))
