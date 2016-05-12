(ns merkledag.data.finance.parse
  "Ledger file parsing code."
  (:require
    (clj-time
      [coerce :as ctime]
      [core :as time]
      [format :as ftime])
    [clojure.java.io :as io]
    [clojure.string :as str]
    [datascript.core :as d]
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

(defn- rand-hex
  [length]
  (let [bs (byte-array length)]
    (.nextBytes (java.security.SecureRandom.) bs)
    (str/join (map (partial format "%02x") bs))))


(defn gen-ident
  "Generates a unique identifier based on the given `:data/type` keyword."
  ([kw]
   (gen-ident kw nil))
  ([kw hex]
   (str/join ":" [(namespace kw) (name kw) (or hex (rand-hex 24))])))


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


(defn- join-field
  [obj field delimiter]
  (let [v (get obj field)]
    (if (sequential? v)
      (assoc obj field (str/join delimiter v))
      obj)))


(defn- lift-meta
  [obj meta-tag field-key f]
  (if-let [value (get-in obj [::meta meta-tag])]
    (-> obj
        (assoc field-key (f value))
        (update ::meta dissoc meta-tag))
    obj))


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
           (types/->Quantity v1 v2)
         (and (symbol? v1) (number? v2))
           (types/->Quantity v2 v1)
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
          :finance.transaction/date date
          ; TODO: should take into account timezone
          :time/at (ctime/to-date-time date)}
         (collect
           {:title                       (collect-one :TxMemo)
            :description                 (collect-all :MetaComment)
            :time/at                     (collect-one :TimeMeta)
            :finance.transaction/status  (collect-one :TxStatus)
            :finance.transaction/code    (collect-one :TxCode)
            :finance.transaction/entries (collect-all :Posting)
            ::meta                       (collect-map :MetaEntry)}
           children)
         (join-field :description "\n")
         (update-time :time/at :finance.transaction/date)
         (lift-meta :UUID :data/ident (partial gen-ident :finance/transaction))))})


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
            {:data/type :finance/posting
             :finance.posting/account (second account)}
            (assoc-some
              :finance.posting/amount amount
              :finance.posting/type posting-type)
            (collect
              {:finance.posting/lot-cost (collect-one :PostingLotCost)
               :finance.posting/lot-date (collect-one :PostingLotDate)
               :finance.posting/price    (collect-one :PostingPrice)
               :finance.posting/balance  (collect-one :PostingBalance)
               ::date                    (collect-one :PostingDate)
               :time/at                  (collect-one :TimeMeta)
               :data/sources             (collect-set :SourceMeta)
               ::meta                    (collect-map :MetaEntry)
               :description              (collect-all :MetaComment)
               :finance.posting/invoice  (collect-all :LineItem)}
              children)
            (update-time :time/at ::date)
            (join-field :description "\n")
            (as-> posting
              (cond-> posting
                (and (or (nil? (first (:form amount)))
                         (zero? (first (:form amount))))
                     (= :balanced-virtual posting-type)
                     (:balance posting))
                  (-> (assoc :finance.posting/type :balance-check)
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



;; ## Data Integration

(def ^:dynamic *book-name*
  "String naming the current set of books being parsed."
  nil)


(defn- entry-dispatch
  "Selects an integration dispatch value based on the argument type."
  [db entry]
  (if (vector? entry)
    (keyword "merkledag.data.finance.parse" (name (first entry)))
    (:data/type entry)))


(defmulti entry-updates
  "Generates and returns a sequence of datums which can be transacted onto the
  database to integrate the given entry."
  #'entry-dispatch)


(defmethod entry-updates :default
  [db entry]
  (println "Ignoring unsupported entry" (entry-dispatch db entry))
  nil)


(defmethod entry-updates ::ignored
  [db header]
  ; Ignored
  nil)


(derive ::CommentHeader ::ignored)
(derive ::CommentBlock  ::ignored)
(derive ::IncludeFile   ::ignored)


(defmethod entry-updates :finance/commodity
  [db commodity]
  (let [code (:finance.commodity/code commodity)
        entity (when db (d/entity db [:finance.commodity/code code]))]
    [(-> commodity
         (dissoc :data/sources ::format ::options)
         (assoc :db/id (:db/id entity -1))
         (cond->
           ;(nil? (:data/ident entity))
           ;  (assoc :data/ident (gen-ident :finance/commodity))
           (and (::format commodity) (not (re-seq #"^\d" (::format commodity))))
             (assoc :finance.commodity/currency-symbol (first (::format commodity)))))]))


(defmethod entry-updates :finance/price
  [db price]
  (let [code  (:finance.price/commodity price)
        value (:finance.price/value price)
        commodity (when db (d/entity db [:finance.commodity/code code]))
        inst (ctime/to-date-time (:time/at price))
        [extant] (d/q '[:find [?p]
                        :in $ ?code ?time
                        :where [?c :finance.commodity/code ?code]
                               [?p :finance.price/commodity ?c]
                               [?p :data/type :finance/price]
                               [?p :time/at ?time]]
                      db code inst)]
    [; Check that the commodity exists, otherwise create it.
     (when-not commodity
       {:db/id -2
        :data/type :finance/commodity
        :finance.commodity/code code})
     ; Check for an extant price point for this commodity.
     {:db/id (:db/id extant -1)
      :data/type :finance/price
      :finance.price/commodity (:db/id commodity -2)
      :finance.price/value value
      :time/at inst}]))


(defmethod entry-updates :finance/account
  [db account]
  (when-not *book-name*
    (throw (RuntimeException. "Must bind *book-name* to integrate accounts!")))
  (let [path (vec (cons *book-name* (:finance.account/path account)))
        [extant] (d/q '[:find [?a]
                        :in $ ?books ?path
                        :where [?a :finance.account/path ?path]
                               [?a :finance.book/name ?books]
                               [?a :data/type :finance/account]]
                      db *book-name* path)]
    [(-> account
         (assoc :db/id (or extant -1)
                :finance.book/name *book-name*
                :finance.account/path path)
         (dissoc :data/sources))]))


(defmethod entry-updates :finance/transaction
  [db transaction]
  (when-not *book-name*
    (throw (RuntimeException. "Must bind *book-name* to integrate transactions!")))
  ; TODO: implement
  [])























(comment
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
                  (if (= :finance/account (:data/type next-node))
                    (merge next-node new-account)
                    (throw (ex-info "Tried to add an account at an existing intermediate node!"
                                    {:new-account new-account, :node next-node})))
                  (if (= :finance/account-group (:data/type next-node))
                    (update next-node :group/children add-account (rest path) new-account)
                    (throw (ex-info "Tried to add an account as a child of a non-group node!"
                                    {:new-account new-account, :node next-node}))))))
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
  )
