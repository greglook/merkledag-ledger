(ns merkledag.data.finance.import
  "Functions to integrate incoming (and incomplete) data into the financial
  database. Import takes a _raw entry_ and the current database and returns a
  set of transactions to import the data represented by the entry."
  (:require
    (clj-time
      [coerce :as ctime]
      [core :as time])
    [clojure.string :as str]
    [datascript.core :as d]
    [merkledag.data.finance.schema :as schema]
    [merkledag.data.finance.types :as types]
    [schema.core :as s]))


(def ^:private ^:dynamic *temp-id*
  "Holds the next available temporary id for use; each use should decrement it
  to provide the next available id."
  nil)


(defmacro with-tx-context
  [& body]
  `(binding [*temp-id* -1]
     ~@body))


(defn- next-temp-id!
  "Returns the next unused temporary id number and registers it with the
  dynamic var."
  []
  (when-not (thread-bound? #'*temp-id*)
    (throw (RuntimeException. "Dynamic var *temp-id* must be bound to use this function")))
  (let [id *temp-id*]
    (set! *temp-id* (dec id))
    id))


(defn- id-or-temp!
  "If the given entity exists and contains a `:db/id` field, the value is
  returned. Otherwise, a temporary id is generated."
  [entity]
  (or (:db/id entity) (next-temp-id!)))


(defn- rand-hex
  [length]
  (let [bs (byte-array length)]
    (.nextBytes (java.security.SecureRandom.) bs)
    (str/join (map (partial format "%02x") bs))))


(defn gen-ident
  "Generates a unique identifier based on the given `:data/type` keyword."
  ([kw]
   (gen-ident kw nil))
  ([kw id]
   (str/join ":" [(namespace kw) (name kw) (or id (rand-hex 24))])))



;; ## Data Integration

(defn import-dispatch
  "Selects an integration dispatch value based on the argument type."
  [db book entry]
  (if (vector? entry)
    (keyword "finance.import" (name (first entry)))
    (:data/type entry)))


(defmulti entry-updates
  "Generates and returns a sequence of datums which can be transacted onto the
  database to integrate the given entry."
  #'import-dispatch)


(defmethod entry-updates :default
  [db book entry]
  (let [entry-type (import-dispatch db book entry)]
    (throw (ex-info (str "Unsupported entry type: " entry-type)
                    {:type entry-type
                     :entry entry}))))


(defmethod entry-updates ::ignored
  [db book header]
  ; Ignored
  nil)


(derive :finance.import/CommentHeader ::ignored)
(derive :finance.import/CommentBlock  ::ignored)
(derive :finance.import/IncludeFile   ::ignored)


(defmethod entry-updates :finance/commodity
  [db _ commodity]
  (s/validate schema/CommodityDefinition commodity)
  (let [code (:finance.commodity/code commodity)
        entity (d/entity db [:finance.commodity/code code])]
    [(-> commodity
         (dissoc :data/sources ::format ::options)
         (assoc :db/id (id-or-temp! entity))
         (cond->
           (and (::format commodity) (not (re-seq #"^\d" (::format commodity))))
             (assoc :finance.commodity/currency-symbol (first (::format commodity)))))]))


(defmethod entry-updates :finance/price
  [db _ price]
  (s/validate schema/CommodityPrice price)
  (let [code  (:finance.price/commodity price)
        value (:finance.price/value price)
        commodity (d/entity db [:finance.commodity/code code])]
    (when-not commodity
      (throw (ex-info (str "Cannot import price for nonexistent commodity " code)
                      {:commodity code
                       :entry price})))
    (let [inst (ctime/to-date-time (:time/at price))
          [extant] (d/q '[:find [?p]
                          :in $ ?code ?time
                          :where [?c :finance.commodity/code ?code]
                                 [?p :finance.price/commodity ?c]
                                 [?p :data/type :finance/price]
                                 [?p :time/at ?time]]
                        db code inst)]
      [{:db/id (id-or-temp! extant)
        :data/type :finance/price
        :finance.price/commodity (:db/id commodity)
        :finance.price/value value
        :time/at inst}])))


(defmethod entry-updates :finance/account
  [db book account]
  (when-not book
    (throw (IllegalArgumentException. "Must provide book name to import accounts!")))
  (s/validate (dissoc schema/AccountDefinition :finance.account/book) account)
  (let [path (:finance.account/path account)
        [extant] (d/q '[:find [?a]
                        :in $ ?book ?path
                        :where [?a :finance.account/path ?path]
                               [?a :finance.account/book ?book]
                               [?a :data/type :finance/account]]
                      db book path)]
    [(assoc account
            :db/id (id-or-temp! extant)
            :finance.account/book book)]))


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
              [before [missing after]]
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


(defmethod entry-updates :finance/transaction
  [db book transaction]
  (when-not book
    (throw (IllegalArgumentException. "Must provide book name to import transactions!")))
  (let [entries (interpolate-entries (:finance.transaction/entries transaction))
        updates (map (partial entry-updates db book) entries)
        entry-ids (set (map (comp :db/id first) updates))]
    (cons
      (-> transaction
          (assoc :db/id (next-temp-id!)
                 :finance.transaction/entries entry-ids)
          (dissoc :data/sources)) ; FIXME: properly link these
      (apply concat updates))))


(defmethod entry-updates ::entry
  [db book entry]
  ; TODO: factor this lookup out?
  (let [account-ref (:finance.entry/account entry)
        attr-key (if (keyword? account-ref)
                   :finance.account/alias
                   :finance.account/path)
        query {:find '[[?a]]
               :in '[$ ?book ?id]
               :where [['?a :finance.account/book '?book]
                       ['?a attr-key '?id]]}
        [account-id] (d/q query db book account-ref)]
    (when-not account-id
      (throw (ex-info (str "No account found matching id: " (pr-str account-ref))
                      {:account account-ref})))
    (cons
      (-> entry
          (assoc :db/id (next-temp-id!)
                 :finance.entry/account account-id)
          (dissoc :data/sources)) ; FIXME: properly link these
      (when-let [invoice (:finance.posting/invoice entry)]
        (entry-updates db book invoice)))))


(derive :finance.entry/note          ::entry)
(derive :finance.entry/open-account  ::entry)
(derive :finance.entry/close-account ::entry)
(derive :finance.entry/balance-check ::entry)
(derive :finance.entry/posting       ::entry)


(defmethod entry-updates :finance/invoice
  [db book invoice]
  (let [item-updates (map (partial entry-updates db book)
                          (:finance.invoice/items invoice))]
    (cons
      (assoc invoice
             :db/id (next-temp-id!)
             :finance.invoice/items (set (map (comp :db/id first) item-updates)))
      (apply concat item-updates))))


(defmethod entry-updates :finance/item
  [db book item]
  [(assoc item :db/id (next-temp-id!))])
