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
    (merkledag.data.finance
      [account :as account]
      [schema :as schema]
      [transaction :as tx]
      [types :as types])
    [schema.core :as s]))


(def ^:private ^:dynamic *tx-context*
  "Holds the next available temporary id for use; each use should decrement it
  to provide the next available id."
  nil)


(defmacro with-context
  [book & body]
  `(binding [*tx-context* {:book ~book, :next-id -1}]
     ~@body))


(defn- next-temp-id!
  "Returns the next unused temporary id number and registers it with the
  dynamic var."
  []
  (when-not (thread-bound? #'*tx-context*)
    (throw (RuntimeException. "Dynamic var *tx-context* must be bound to use this function")))
  (let [id (:next-id *tx-context*)]
    (set! *tx-context* (update *tx-context* :next-id dec))
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
  [db entry]
  (if (vector? entry)
    (keyword "finance.import" (name (first entry)))
    (:data/type entry)))


(defmulti entry-updates
  "Generates and returns a sequence of datums which can be transacted onto the
  database to integrate the given entry. The first update in the list should
  correspond to the entry in the argument."
  #'import-dispatch)


(defmethod entry-updates :default
  [db entry]
  (let [entry-type (import-dispatch db entry)]
    (throw (ex-info (str "Unsupported entry type: " entry-type)
                    {:type entry-type
                     :entry entry}))))


(defmethod entry-updates ::ignored
  [db ignored]
  ; Ignored
  nil)


(derive :finance.import/CommentHeader ::ignored)
(derive :finance.import/CommentBlock  ::ignored)
(derive :finance.import/IncludeFile   ::ignored)


(defmethod entry-updates :finance/commodity
  [db commodity]
  (s/validate schema/CommodityDefinition commodity)
  (let [code (:finance.commodity/code commodity)
        entity (d/entity db [:finance.commodity/code code])]
    [(assoc commodity :db/id (id-or-temp! entity))]))


(defmethod entry-updates :finance/price
  [db price]
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
      [{:db/id (id-or-temp! {:db/id extant})
        :data/type :finance/price
        :finance.price/commodity (:db/id commodity)
        :finance.price/value value
        :time/at inst}])))


(defmethod entry-updates :finance/account
  [db account]
  (when-not (:book *tx-context*)
    (throw (ex-info "Context must provide book name to import accounts!"
                    {:context *tx-context*})))
  (s/validate (dissoc schema/AccountDefinition :finance.account/book) account)
  (let [book (:book *tx-context*)
        path (:finance.account/path account)
        extant (account/find-account db book path)]
    (when extant
      (printf "%s %s -> %d (%s)\n"
              book (pr-str path) (:db/id extant) (:finance.account/book extant)))
    [(assoc account
            :db/id (id-or-temp! extant)
            :finance.account/book book)]))


(defmethod entry-updates :finance/transaction
  [db transaction]
  (s/validate schema/Transaction transaction)
  ; TODO: need to do deduplication here
  (let [entries (tx/interpolate-entries (:finance.transaction/entries transaction))
        updates (map (partial entry-updates db) entries)
        entry-ids (set (map (comp :db/id first) updates))]
    (cons
      (-> transaction
          (assoc :db/id (next-temp-id!)
                 :finance.transaction/entries entry-ids)
          (dissoc :data/sources)) ; FIXME: properly link these
      (apply concat updates))))


(defmethod entry-updates ::entry
  [db entry]
  (when-not (:book *tx-context*)
    (throw (ex-info "Context must provide book name to import transaction entries!"
                    {:context *tx-context*})))
  (let [book (:book *tx-context*)
        account (account/find-account! db book (:finance.entry/account entry))]
    (cons
      (-> entry
          (assoc :db/id (next-temp-id!)
                 :finance.entry/account (:db/id account))
          (dissoc :data/sources)) ; FIXME: properly link these
      (when-let [invoice (:finance.posting/invoice entry)]
        (entry-updates db invoice)))))


(derive :finance.entry/note          ::entry)
(derive :finance.entry/open-account  ::entry)
(derive :finance.entry/close-account ::entry)
(derive :finance.entry/balance-check ::entry)
(derive :finance.entry/posting       ::entry)


(defmethod entry-updates :finance/invoice
  [db invoice]
  (let [item-updates (map (partial entry-updates db)
                          (:finance.invoice/items invoice))]
    (cons
      (assoc invoice
             :db/id (next-temp-id!)
             :finance.invoice/items (set (map (comp :db/id first) item-updates)))
      (apply concat item-updates))))


(defmethod entry-updates :finance/item
  [db item]
  [(assoc item :db/id (next-temp-id!))])
