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
  [obj]
  (if-let [t (:time obj)]
    (update obj :time
      #(if (and (vector? %) (= :Time (first %)))
         (let [[_ time zone] %]
           (parse-time (:date obj) time zone))
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
       [:Number (BigDecimal. (str/join digits))])

   :Percentage
     (fn ->percentage
       [number]
       [:% (/ (second number) 100)])})


(def commodity-transforms
  {:CommodityCode
     (fn ->commodity-code
       [code]
       [:CommodityCode
        (if (= "$" code) 'USD (symbol code))])

   :Quantity
     (fn ->quantity
       [& children]
       (when (not= '("0") children)
         (let [cfg (into {} children)]
           (quantity/->Quantity (:Number cfg) (:CommodityCode cfg)))))

   ; FIXME: implement
   ;:CommodityDefinition identity
   ;:CommodityConversion identity
   ;:CommodityPrice identity
   })


(def account-transforms
  {:AccountPathSegment (comp str/join list)
   :AccountPath vector
   :AccountAlias keyword

   ; TODO: Account Definitions
   })


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
       [:Transaction
        (->
          {:date date}
          (collect
            {:title    (collect-one :TxMemo)
             :status   (collect-one :TxStatus)
             :code     (collect-one :TxCode)
             :time     (collect-one :TimeMeta)
             :meta     (collect-map :MetaEntry)
             :comments (collect-all :MetaComment)
             :postings (collect-all :Posting)}
            children)
          (update-time))])})


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
            (update-time)
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
  (parse/transform ledger-transforms tree))



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
  "Adds a `:parse/source` key to associative values in a parsed entry."
  [source entry]
  (if (associative? (second entry))
    (update entry 1 assoc :parse/source (apply subs source (parse/span entry)))
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
  [file]
  (->> file
       (io/file)
       (io/reader)
       (line-seq)
       (group-lines)
       (mapcat parse-group)))


(defn parse-files
  "Parse many ledger files to populate a new data structure."
  ([files]
   (parse-files {} files))
  ([data files]
   (reduce parse-file data files)))
