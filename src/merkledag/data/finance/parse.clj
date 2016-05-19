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
  (-> (if zone
        (ftime/with-zone time-format zone)
        time-format)
      (ftime/parse (str date "T" time))))


(defn- update-time
  "Given an object with date information and potentially some time info, update
  the time attribute with the date information or set it to the start of the
  object's date."
  [obj time-key date-key]
  (if-let [date (get obj date-key)]
    (let [t (get obj time-key)]
      (assoc obj
        time-key
        (if (and (vector? t) (= :Time (first t)))
          (let [[_ time-str zone] t]
            (parse-time date time-str zone))
          (or t (time/from-time-zone (ctime/to-date-time date)
                                     (time/default-time-zone))))))
    obj))


(defn- join-field
  [obj field delimiter]
  (let [v (get obj field)]
    (if (sequential? v)
      (assoc obj field (str/join delimiter v))
      obj)))


(defn- lift-meta
  ([obj meta-tag]
   (lift-meta obj meta-tag meta-tag))
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


(defn- distribute-attr
  "Distributes an attribute from a parent entity into a set of child entities,
  keeping any values already present."
  [x attr-key coll-key]
  (if-let [x-attr (get x attr-key)]
    (update
      x coll-key
      (partial mapv
               (fn [entry]
                 (if (some? (get entry attr-key))
                   entry
                   (assoc entry attr-key x-attr)))))
    x))


(defn- add-ranks
  "Add rank attributes to a sequence in a sub-key."
  [x coll-key rank-key]
  (update
    x coll-key
    (partial mapv
             (fn [i entry]
               (assoc entry rank-key i))
             (range))))


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
      (when-let [bad-children (seq (filter #(not= 2 (count %)) matches))]
        (throw (ex-info
                 (str "Cannot unbox " (count bad-children) " " (pr-str k)
                      " children which have the wrong entry counts")
                 {:key k, :bad-children bad-children})))
      (into {} (map second matches)))))


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
     (->
       {:data/type :finance/commodity
        :finance.commodity/code code}
       (collect
         {:title     (collect-one :NoteDirective)
          :data/tags (collect-map :MetaDirective)
          ::format   (collect-one :CommodityFormat)
          ::options  (collect-all :CommodityOption)}
         children)
       (lift-meta :type :finance.commodity/type (partial keyword "finance.commodity.type"))
       ; TODO: load asset class and sectors
       (as-> commodity
         (let [fmt (::format commodity)]
           (if (and fmt (not (re-seq #"^\d" fmt)))
             (assoc commodity :finance.commodity/currency-symbol (subs fmt 0 1))
             commodity)))
       ; These are unused at the moment.
       (dissoc ::format ::options)))

   :CommodityPrice
   (fn ->commodity-price
     [date code price]
     {:data/type :finance/price
      :time/at (ctime/to-date-time date)
      :finance.price/commodity code
      :finance.price/value price})})


(def account-transforms
  {:AccountPathSegment (comp str/join list)
   :AccountPath vector
   :AccountAlias keyword

   :AccountDefinition
   (fn ->account-definition
     [path & children]
     (->
       {:data/type :finance/account
        :title (last path)
        :finance.account/path path}
       (collect
         {:finance.account/alias (collect-one :AccountAliasDirective)
          ::assertion            (collect-one :AccountAssertion)
          :description           (collect-all :NoteDirective)
          :data/tags             (collect-map :MetaDirective)}
         children)
       (join-field :description "\n")
       (lift-meta :title)
       (lift-meta :type :finance.account/type (partial keyword "finance.account.type"))
       (lift-meta :external-id :finance.account/external-id)
       (lift-meta :link :finance.account/links hash-set)
       (as-> account
         (if-let [commodities (some->>
                                (::assertion account)
                                (re-seq #"commodity == \"(\S+)\"")
                                (map (comp (commodity-transforms :CommodityCode) second))
                                (set))]
           (assoc account :finance.account/allowed-commodities commodities)
           account))
       (dissoc ::assertion)))})


(def metadata-transforms
  {:TagName keyword
   :MetaTag
   (fn ->meta
     ([k]   [k true])
     ([k v] [k v]))

   :SourceMeta
   (fn ->source-meta
     [src line]
     [:SourceMeta [src line]])})


(def transaction-transforms
  {:Transaction
   (fn ->transaction
     [date & children]
     (-> {:data/type :finance/transaction
          :finance.transaction/date date}
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
         (distribute-attr :time/at :finance.transaction/entries)
         (dissoc :time/at)
         (add-ranks :finance.transaction/entries :finance.entry/rank)))

   :TxFlag
   (fn ->flag
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
            {:finance.entry/source-lines (collect-set :SourceMeta)
             :finance.balance/amount     (collect-one :PostingBalance)
             :finance.posting/cost       (collect-one :PostingLotCost)
             :finance.posting/lot-date   (collect-one :PostingLotDate)
             :finance.posting/price      (collect-one :PostingPrice)
             :finance.posting/invoice    (collect-all :LineItem)
             ::date                      (collect-one :PostingDate)
             :time/at                    (collect-one :TimeMeta)
             :data/tags                  (collect-map :MetaEntry)
             :description                (collect-all :MetaComment)}
            children)
          ; TODO: (lift-meta :interval :time/interval ...)
          ; Default :time/at to the start of :time/interval if missing
          (update-time :time/at ::date)
          (dissoc ::date)
          (join-field :description "\n")
          (lift-meta :Payee :finance.posting/payee)
          (lift-meta :external-id :finance.entry/external-id)
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
  "Adds metadata to the entry with the original parse text."
  [source entry]
  (vary-meta entry assoc ::source (apply subs source (parse/span entry))))


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
  [file]
  (->> file
       (io/file)
       (io/reader)
       (line-seq)
       (group-lines)
       (mapcat parse-group)))
