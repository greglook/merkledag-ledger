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
    [merkledag.data.finance.quantity :as quantity]))


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
  (time/to-time-zone
    (ftime/parse
      (if zone
        (ftime/with-zone time-format zone)
        time-format)
      (str date "T" time))
    time/utc))


(defn- update-time
  [obj time-key date-key]
  (if-let [t (get obj time-key)]
    (update obj time-key
      #(if (and (vector? %) (= :Time (first %)))
         (let [[_ time zone] %]
           (parse-time (get obj date-key) time zone))
         %))
    obj))


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
    (when-let [matches (seq (filter #(and (vector? %)
                                          (= k (first %)))
                                    children))]
      (when-let [bad-children (seq (filter #(< 2 (count %)) matches))]
        (throw (ex-info
                 (str "Cannot unbox " (count bad-children) " " (pr-str k)
                      " children which have too many entries")
                 {:key k, :bad-children bad-children})))
      (mapv second matches))))


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
       [(/ number 100) '%])})


(def commodity-transforms
  {:CommodityCode
     (fn ->commodity-code
       [code]
       (if (= "$" code) 'USD (symbol code)))

   :Quantity
     (fn ->quantity
       [& [v1 v2 :as children]]
       (cond
         (and (= "0" v1) (nil? v2))
           nil
         (and (number? v1) (symbol? v2))
           (quantity/->Quantity v1 v2)
         (and (symbol? v1) (number? v2))
           (quantity/->Quantity v2 v1)
         :else
           (throw (ex-info (str "Unknown quantity format! " (pr-str [v1 v2]))
                           {:form children}))))

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
        :finance.price/value price})

   ; Not supported:
   ;:CommodityConversion identity
   })


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
                     ::path path}
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
     (fn ([k]   [:MetaEntry k true])
         ([k v] [:MetaEntry k v]))

   :SourceMeta
     (fn [src line]
       [:SourceMeta {:source src, :line line}])})


(def transaction-transforms
  {:TxStatus
     (fn [chr]
       [:TxStatus (case chr "!" :pending, "*" :cleared, :uncleared)])

   :Transaction
     (fn [date & children]
       (->
         {:data/type :finance/transaction
          :time/at (ctime/to-date-time date)
          ::date date}
         (collect
           {:title                       (collect-one :TxMemo)
            :description                 (collect-all :MetaComment)
            :time/at                     (collect-one :TimeMeta)
            :finance.transaction/status  (collect-one :TxStatus)
            :finance.transaction/code    (collect-one :TxCode)
            :finance.transaction/meta    (collect-map :MetaEntry)
            :finance.transaction/entries (collect-all :Posting)}
           children)
         (as-> data
           (if (:description data)
             (update data :description (partial str/join "\n"))
             data))
         ; TODO: pull UUID metadata out
         (update-time :time/at ::date)))})


(def posting-transforms
  {:Posting
     (fn [account & children]
       (let [posting-type (case (first account)
                            :RealAccountRef :real
                            :VirtualAccountRef :virtual
                            :BalancedVirtualAccountRef :balanced-virtual)
             [amount children] (if (vector? (first children))
                                 [nil children]
                                 [(first children) (rest children)])]
         [:Posting
          (->
            {:account (second account)}
            (cond->
              amount
                (assoc :amount amount)
              (not= posting-type :real)
                (assoc :type posting-type))
            (collect
              {:lot-cost (collect-one :PostingLotCost)
               :lot-date (collect-one :PostingLotDate)
               :price    (collect-one :PostingPrice)
               :balance  (collect-one :PostingBalance)
               :date     (collect-one :PostingDate)
               :time     (collect-one :TimeMeta)
               :sources  (collect-all :SourceMeta)
               :meta     (collect-map :MetaEntry)
               :comments (collect-all :MetaComment)
               :items    (collect-all :LineItem)}
              children)
            (update-time :time :date)
            (as-> posting
              (cond-> posting
                (and (or (nil? (first (:form amount)))
                         (zero? (first (:form amount))))
                     (= :balanced-virtual posting-type)
                     (:balance posting))
                  (-> (assoc :type :balance-check)
                      (dissoc :amount)))))]))

   :LineItemTaxGroup keyword
   :LineItemTaxGroups
     (fn [& groups] [:LineItemTaxGroups (set groups)])

   :LineItem
     (fn [desc & children]
       [:LineItem
        (collect
          {:title desc}
          {:total       (collect-one :LineItemTotal)
           :amount      (collect-one :LineItemAmount)
           :price       (collect-one :LineItemPrice)
           :tax-groups  (collect-one :LineItemTaxGroups)
           :tax-applied (collect-one :LineItemTaxApplied)}
          children)])})


(def ledger-transforms
  (merge basic-transforms
         commodity-transforms
         account-transforms
         metadata-transforms
         transaction-transforms
         posting-transforms))


(defn interpret-parse
  [tree]
  (try
    (parse/transform ledger-transforms tree)
    (catch Exception e
      (throw (ex-info (str "Failed to interpret parse tree: " e)
                      {:tree tree})))))



;; ## Data Integration

(def ^:dynamic *book-name*
  "String naming the current set of books being parsed."
  nil)


(defn assoc-some
  ([x k v]
   (if (some? v)
     (assoc x k v)
     x))
  ([x k v & more]
   (->> more
        (partition-all 2)
        (cons [k v])
        (reduce #(assoc-some %1 (first %2) (second %2)) x))))


(defn integrater-dispatch
  "Selects an integration dispatch value based on the argument type."
  [_ entry]
  (if (vector? entry)
    (first entry)
    (:data/type entry)))


(defmulti integrate-entry
  "Integrates the given entry into a data system."
  #'integrater-dispatch)


(defmethod integrate-entry :default
  [data entry]
  (println "Ignoring unsupported entry" (integrater-dispatch data entry))
  data)


(defmethod integrate-entry :CommentHeader
  [data entry]
  ; Ignored
  data)


(defmethod integrate-entry :CommentBlock
  [data entry]
  ; Ignored
  data)


(defmethod integrate-entry :finance/commodity
  [data entry]
  (let [code (:finance.commodity/code entry)
        current (get-in data [:commodities code])
        new-data (merge current (dissoc entry ::format ::options))]
    (if (= current new-data)
      data
      (assoc-in data [:commodities code] new-data))))


(defmethod integrate-entry :finance/price
  [data entry]
  (let [code (:finance.price/commodity entry)
        year-path [:prices code (str (time/year (:time/at entry)))]
        prices (or (get-in data year-path)
                   {:data/type :finance/price-history
                    :finance.price/commodity code
                    :finance.price/points []})
        new-price {:time (ctime/to-date-time (:time/at entry))
                   :value (:finance.price/value entry)}
        new-prices (update prices :finance.price/points
                           #(->> (conj % new-price) (set) (sort-by :time) (vec)))]
    (assoc-in data year-path new-prices)))


(defn get-account
  "Looks up an account by path, starting from the given set of accounts which
  are children of the current node."
  [accounts path]
  (when-let [account (first (filter #(= (first path) (:title %)) accounts))]
    (if-let [remaining (seq (rest path))]
      (recur (:group/children account) remaining)
      account)))


(defn add-account
  "Merges a new account definition into a set of accounts, replacing any
  account at the same path and adding to sets where necessary."
  [accounts path new-account]
  (if-let [next-node (get-account accounts [(first path)])]
    ; node exists, merge
    (-> accounts
        (disj next-node)
        (conj (if (empty? path)
                new-account
                (update next-node :group/children add-account (rest path) new-account))))
    ; node does not exist, create and recurse
    (conj
      (set accounts)
      (if (empty? path)
        new-account
        {:title (first path)
         :data/type :finance/account-group
         :group/children (add-account nil (rest path) new-account)}))))


(defmethod integrate-entry :finance/account
  [data entry]
  (when-not *book-name*
    (throw (RuntimeException. "Must bind *book-name* to integrate accounts!")))
  (let [path (::path entry)
        current-data (get-account (get-in data [:books *book-name* :accounts]) path)
        new-data (merge current-data (dissoc entry ::path))]
    (if (= current-data new-data)
      data
      (update-in data [:books *book-name* :accounts]
                 add-account (butlast path) new-data))))


(defmethod integrate-entry :finance/transaction
  [data entry]
  (when-not *book-name*
    (throw (RuntimeException. "Must bind *book-name* to integrate transactions!")))
  (let [date (::date entry)
        entry (dissoc entry ::date)]
    (update-in data
      [:books *book-name* :ledger
       (str (time/year date))
       (format "%02d" (time/month date))
       (format "%02d" (time/day date))]
      (fnil update {:data/type :finance/ledger
                    :time/date date
                    :finance.ledger/transactions []})
      :finance.ledger/transactions
      #(vec (sort-by :time/at (conj % entry))))))


; TODO: (transaction-seq ledger from to)



;; ## File Parsing

(defn group-lines
  "Takes a sequence of lines and returns a new sequence of groups of lines
  which were separated by blank lines in the input."
  [lines]
  (->> lines
       (partition-by #{""})
       (remove #(every? #{""} %))
       (map #(str (str/join "\n" %) "\n"))))


(defn add-source
  "Adds a `:data/sources` key to associative values in a parsed entry."
  [source entry]
  (if (map? entry)
    (update entry :data/sources (fnil conj #{}) (apply subs source (parse/span entry)))
    entry))


(defn parse-group
  "Parses a group of lines from a file."
  [group]
  (->> group
       (ledger-parser)
       (interpret-parse)
       (map (partial add-source group))))


(defn parse-file
  "Parse a single file, returning a sequence of ledger entries."
  [data file]
  (->> file
       (io/file)
       (io/reader)
       (line-seq)
       (group-lines)
       (mapcat parse-group)
       (reduce integrate-entry data)))


(defn parse-files
  "Parse many ledger files to populate a new data structure."
  ([files]
   (parse-files {} files))
  ([data files]
   (reduce parse-file data files)))
