(ns finance.format.ledger.parse
  "Ledger file parsing code."
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [finance.data.account :as account]
    [finance.data.balance :as balance]
    [finance.data.commodity :as commodity]
    [finance.data.core :as data]
    [finance.data.entry :as entry]
    [finance.data.item :as item]
    [finance.data.posting :as posting]
    [finance.data.price :as price]
    [finance.data.quantity :as quantity]
    [finance.data.time :as time]
    [finance.data.transaction :as transaction]
    [instaparse.core :as parse]
    [tick.alpha.api :as t]))


(defn- load-grammar
  "Loads and parses the grammar resource."
  []
  (parse/parser (io/resource "finance/format/ledger/grammar.bnf")))


(def ledger-parser
  (delay (load-grammar)))


;; ## Parse Helpers

(defn- parse-time
  [date time-str zone]
  (if time-str
    (let [inst (t/parse (str date "T" time-str))]
      (if zone
        (t/in inst zone)
        inst))
    (t/parse date)))


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
               (or t (t/midnight date)))))
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
   (if-let [value (get-in obj [::data/tags meta-tag])]
     (-> obj
         (assoc field-key (f value))
         (update ::data/tags dissoc meta-tag)
         (as-> obj'
           (if (empty? (::data/tags obj'))
             (dissoc obj' ::data/tags)
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
  {:Date
   t/parse

   :DateTime
   (fn ->date-time
     [date [_ time-str zone]]
     (parse-time date time-str zone))

   :TimeZone
   t/zone

   :Number
   (fn ->number
     [& digits]
     (BigDecimal. (str/join digits)))

   :Percentage
   (fn ->percentage
     [number]
     (/ number 100))})


(def commodity-transforms
  {:CommodityCode
   (fn ->commodity-code
     [code]
     (if (= "$" code)
       'USD
       (symbol code)))

   :CommodityDefinition
   (fn ->commodity
     [code & children]
     (->
       {::data/type :finance.data/commodity
        ::commodity/code code}
       (collect
         {::data/tags      (collect-map :MetaDirective)
          ::commodity/name (collect-one :NoteDirective)
          ::format         (collect-one :CommodityFormat)
          ::options        (collect-all :CommodityOption)}
         children)
       (lift-meta :type ::commodity/type keyword)
       (lift-meta :class ::commodity/class keyword)
       (lift-meta :sector ::commodity/sector keyword)
       (as-> commodity
         (if-not (::commodity/name commodity)
           (assoc commodity ::commodity/name (str code))
           commodity)
         (let [fmt (::format commodity)]
           (if (and fmt (not (re-seq #"^\d" fmt)))
             (assoc commodity ::commodity/currency-symbol (subs fmt 0 1))
             commodity)))
       ;; These are unused at the moment.
       (dissoc ::format ::options)))

   :CommodityPrice
   (fn ->commodity-price
     [date-time code price]
     {::data/type :finance.data/price
      ::price/time (if (t/date? date-time)
                     (t/midnight date-time)
                     date-time)
      ::price/commodity code
      ::price/value price})

   :Quantity
   (fn ->quantity
     [& [v1 v2 :as children]]
     (cond
       (and (= "0" v1) (nil? v2))
       nil

       (and (number? v1) (symbol? v2))
       (quantity/q v1 v2)

       (and (symbol? v1) (number? v2))
       (quantity/q v2 v1)

       :else
       (throw (ex-info (str "Unknown quantity format! " (pr-str [v1 v2]))
                       {:form children}))))})


(def account-transforms
  {:AccountPathSegment (comp str/join list)
   :AccountPath vector
   :AccountAlias keyword

   :AccountDefinition
   (fn ->account-definition
     [path & children]
     (->
       {::data/type :finance.data/account
        ::account/title (last path)
        ::account/path path}
       (collect
         {::account/alias (collect-one :AccountAliasDirective)
          ::account/description (collect-all :NoteDirective)
          ::data/tags (collect-map :MetaDirective)
          ::assertion (collect-one :AccountAssertion)}
         children)
       (join-field ::account/description "\n")
       (lift-meta ::account/title)
       (lift-meta :type ::account/type keyword)
       (lift-meta :external-id ::account/external-id)
       (lift-meta :link ::account/links set)
       (as-> account
         (if-let [commodities (some->>
                                (::assertion account)
                                (re-seq #"commodity == \"(\S+)\"")
                                (map (comp (commodity-transforms :CommodityCode) second))
                                (set))]
           (assoc account ::account/commodities commodities)
           account))
       (dissoc ::assertion)))})


(def metadata-transforms
  {:TagName
   keyword

   :MetaTag
   (fn ->meta
     ([k]
      [k true])
     ([k v]
      [k v]))

   :SourceMeta
   (fn ->source-meta
     [src line]
     [:SourceMeta [src line]])})


(def transaction-transforms
  {:Transaction
   (fn ->transaction
     [date & children]
     (-> {::data/type :finance.data/transaction
          ::transaction/date date}
         (collect
           {::transaction/title       (collect-one :TxMemo)
            ::transaction/description (collect-all :MetaComment)
            ::time                    (collect-one :TimeMeta)
            ::transaction/flag        (collect-one :TxFlag)
            ;::transaction/code        (collect-one :TxCode)
            ::entries                 (collect-all :Posting)
            ::data/tags               (collect-map :MetaEntry)}
           children)
         (join-field ::transaction/description "\n")
         (update ::entries vec)
         (update-time ::time ::transaction/date)
         (lift-meta :UUID ::data/ident (partial str "finance:transaction:"))
         (lift-meta :link ::transaction/links hash-set)
         (distribute-attr ::time ::entries)
         (dissoc ::time)))

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
          {::data/type :finance.data.entry/posting
           ::entry/account (second account)}
          (assoc-some
            ::posting/amount amount)
          (collect
            {::entry/date         (collect-one :PostingDate)
             ::entry/time         (collect-one :TimeMeta)
             ::entry/description  (collect-all :MetaComment)
             ::entry/source-lines (collect-set :SourceMeta)
             ::posting/price      (collect-one :PostingPrice)
             ::balance/amount     (collect-one :PostingBalance)
             ::line-items         (collect-all :LineItem)
             ::lot-cost           (collect-one :PostingLotCost)
             ::lot-date           (collect-one :PostingLotDate)
             ::data/tags          (collect-map :MetaEntry)}
            children)
          ;; TODO: (lift-meta :interval :time/interval ...)
          ;; Default :time/at to the start of :time/interval if missing
          (update-time ::entry/time ::date)
          (join-field ::entry/description "\n")
          (lift-meta :type ::data/type (partial keyword "finance.data.entry"))
          (lift-meta :Payee ::posting/payee)
          (lift-meta :external-id ::entry/external-id)
          (as-> posting
            (cond-> posting
              (::lot-cost posting)
              (update ::posting/cost assoc :amount (::lot-cost posting))

              (::lot-date posting)
              (update ::posting/cost assoc :date (::lot-date posting))

              (= posting-type :virtual)
              (assoc ::posting/virtual? true)

              ;; Automatically detect balance-check entries.
              (and (or (nil? (:value amount))
                       (zero? (:value amount)))
                   (= :balanced-virtual posting-type)
                   (:finance.balance/amount posting))
              (-> (assoc :data/type :finance.entry/balance-check)
                  (dissoc :finance.posting/amount))

              ;; If type is overridden and amount is zero, remove it.
              (and (not= ::entry/posting (::data/type posting))
                   (or (nil? (:value amount)) (zero? (:value amount))))
              (dissoc ::posting/amount)))
          (dissoc ::lot-cost ::lot-date))]))

   :LineItemTaxGroup
   keyword

   :LineItemTaxGroups
   (fn ->tax-groups
     [& groups]
     [:LineItemTaxGroups (set groups)])

   :LineItem
   (fn ->line-item
     [desc & children]
     [:LineItem
      (collect
        {::data/type :finance.data/item
         ::item/title desc}
        {::item/total       (collect-one :LineItemTotal)
         ::item/amount      (collect-one :LineItemAmount)
         ::item/price       (collect-one :LineItemPrice)
         ::item/tax-groups  (collect-one :LineItemTaxGroups)
         ::item/tax-applied (collect-one :LineItemTaxApplied)}
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


;; ## Text Parsing

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
  "Takes a sequence of lines and returns a new lazy sequence of groups of lines
  which were separated by blank lines in the input."
  [lines]
  (->> lines
       (partition-by #{""})
       (remove #(every? #{""} %))
       (map #(str (str/join "\n" %) "\n"))))


(defn parse-group
  "Parses a group of lines from a file, returning an interpreted ledger entry.
  Throws an exception if the parser fails."
  [text]
  (let [parse @ledger-parser]
    (->> (parse text)
         (check-parse!)
         (interpret-parse)
         (map (partial add-source text)))))


(defn parse-lines
  "Parse a sequence of lines into a (lazy) sequence of entity maps."
  [lines]
  (->> lines
       (group-lines)
       (mapcat parse-group)))
