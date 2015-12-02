(ns merkledag.data.finance.core
  "..."
  (:require
    (clj-time
      [coerce :as ctime]
      [core :as time]
      [format :as ftime])
    [clojure.java.io :as io]
    [clojure.string :as str]
    [instaparse.core :as parse]))


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


(defn interpret-parse
  [tree]
  (parse/transform
    {:Number (fn [& digits] [:number (BigDecimal. (str/join digits))])
     :Percentage (fn [number] [:% (/ (second number) 100)])
     :Date ftime/parse-local-date
     :DateTime (fn [date [_ time tz]]
                 (parse-time date time tz))
     :TimeZone (fn [zone]
                 (case zone
                   "Z" time/utc
                   (time/time-zone-for-id zone)))

     :CommodityCode
       (fn [code]
         [:commodity-code
          (if (= "$" code) 'USD (symbol code))])

     :Quantity
       (fn [& children]
         (when (not= '("0") children)
           (let [cfg (into {} children)]
             (tagged-literal 'finance/$
                             [(:number cfg)
                              (:commodity-code cfg)]))))

     :AccountPathSegment (fn [& words] (str/join words))
     :AccountPath vector
     :AccountAlias keyword

     :PostingSource
       (fn [src line]
         [:posting/source {:source (keyword src), :line line}])
     :PostingMeta
      (fn ([k]   [:posting/meta (keyword k) true])
          ([k v] [:posting/meta (keyword k) v]))

     :LineItemTaxGroup keyword
     :LineItemTaxGroups (fn [& groups] [:item/tax-groups (set groups)])
     :LineItem
       (fn [desc & children]
         [:posting/line-item
          (collect
            {:title desc}
            {:amount (collect-one :LineItemAmount)
             :quantity (collect-one :LineItemQuantity)
             :cost (collect-one :LineItemCost)
             :tax-groups (collect-one :item/tax-groups)
             :tax-applied (collect-one :LineItemTaxApplied)}
            children)])

     :Posting
       (fn [account & [amount & children]]
         (let [posting-type (case (first account)
                              :RealAccountRef :real
                              :VirtualAccountRef :virtual
                              :BalancedVirtualAccountRef :balanced-virtual)]
           [:tx/posting
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
                 :meta     (collect-map :posting/meta)
                 :sources  (collect-all :posting/source)
                 :items    (collect-all :posting/line-item)
                 :comments (collect-all :PostingComment)}
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

     :TxMeta (fn ([k]   [:tx/meta (keyword k) true])
                 ([k v] [:tx/meta (keyword k) v]))
     :TxStatus (fn [chr]
                 [:tx/status (case chr "!" :pending, "*" :cleared, :uncleared)])

     :Transaction
       (fn [date & children]
         ; TODO: save substring that was parsed into this tx as a source
         [:Transaction
          (->
            {:date date}
            (collect
              {:title    (collect-one :TxMemo)
               :time     (collect-one :TimeMeta)
               :status   (collect-one :tx/status)
               :meta     (collect-map :tx/meta)
               :comments (collect-all :TxComment)
               :postings (collect-all :tx/posting)}
              children)
            (update-time))])}
    tree))


(defn group-lines
  "Takes a sequence of lines and returns a new sequence of groups of lines
  which were separated by blank lines in the input."
  [lines]
  (->> lines
       (partition-by #{""})
       (remove #(every? #{""} %))
       (map #(str (str/join "\n" %) "\n"))))


(defn parse-file
  [data file]
  (->> (io/file file)
       (io/reader)
       (line-seq)
       (group-lines)
       (mapcat (comp interpret-parse ledger-parser))))


(defn parse-files
  [& files]
  (loop [fs (seq files)
         data {}]
    (if-let [f (first fs)]
      (do (println "Parsing file" f)
          (printf "  (%d lines)\n" (count (line-seq (io/reader (io/file f))))))
      data)))
