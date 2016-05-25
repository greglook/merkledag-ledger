(ns merkledag.data.finance.load
  "Functions to load structured data into the financial database. Loading takes
  an _entry_ and the current database and returns a set of transactions to add
  the data represented by the entry. This is distinct from _importing_ data,
  which involves parsing raw CSV lines and such."
  (:require
    [clj-time.core :as time]
    [clojure.string :as str]
    [datascript.core :as d]
    (merkledag.data.finance
      [account :as account]
      [entry :as entry]
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
  "If the given entity exists and is either a numeric id or contains a `:db/id`
  entry, the id is returned. Otherwise, this function generates and returns a
  temporary id."
  [entity]
  (cond
    (number? entity) entity
    (:db/id entity)  (:db/id entity)
    :else            (next-temp-id!)))


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

(defn entry-dispatch
  "Selects an integration dispatch value based on the argument type."
  [db entry]
  (if (vector? entry)
    (keyword "finance.import" (name (first entry)))
    (:data/type entry)))


(defmulti entry-updates
  "Generates and returns a sequence of datums which can be transacted onto the
  database to integrate the given entry. The first update in the list should
  correspond to the entry in the argument."
  #'entry-dispatch)


(defn load-entry!
  "Loads the interpreted entry into the given database connection. Throws an
  exception if generating or transacting the updates fails."
  [conn book entry]
  (with-context book
    (when-let [updates (->> (entry-updates @conn entry)
                            (remove nil?)
                            (seq))]
      (d/transact! conn updates))))


(defmethod entry-updates :default
  [db entry]
  (let [entry-type (entry-dispatch db entry)]
    (throw (ex-info (str "Unsupported entry type: " entry-type)
                    {:type entry-type
                     :entry entry}))))


(defmethod entry-updates nil
  [db _]
  ; Ignored
  nil)


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
    [{:db/id (next-temp-id!)
      :data/type :finance/price
      :finance.price/commodity (:db/id commodity)
      :finance.price/value value
      :time/at (:time/at price)}]))


(defmethod entry-updates :finance/account
  [db account]
  (when-not (:book *tx-context*)
    (throw (ex-info "Context must provide book name to import accounts!"
                    {:context *tx-context*})))
  (s/validate schema/AccountDefinition account)
  (let [book (:book *tx-context*)
        path (:finance.account/path account)
        extant (account/find-account db book path)]
    (when extant
      (printf "WARN: duplicate account declaration in book %s for account %s (%d)\n"
              book (str/join ":" path) (:db/id extant)))
    [(assoc account
            :db/id (id-or-temp! extant)
            :finance.account/book book)]))


(defmethod entry-updates :finance/transaction
  [db transaction]
  (s/validate schema/Transaction transaction)
  (let [entries (entry/interpolate-amounts (:finance.transaction/entries transaction))
        updates (map (partial entry-updates db) entries)
        entry-ids (map (comp :db/id first) updates)]
    (cons
      (assoc transaction
             :db/id (next-temp-id!)
             :finance.transaction/entries (set entry-ids)
             :finance.transaction/entry-order (vec entry-ids))
      (apply concat updates))))


(defn- update-transaction-entry
  "Applies common updates to a transaction entry. Returns the entry value with
  a temporary `:db/id` and corrected account reference."
  [db entry]
  (when-not (:book *tx-context*)
    (throw (ex-info "Context must provide book name to import transaction entries!"
                    {:context *tx-context*})))
  (let [account-ref (:finance.entry/account entry)
        account (account/find-account! db (:book *tx-context*) account-ref)
        entry' (assoc entry
                      :db/id (next-temp-id!)
                      :finance.entry/account (:db/id account))]
    (if (:finance.entry/rank entry')
      entry'
      (let [[rank] (d/q '[:find [(count ?e)]
                          :in $ ?account ?time
                          :where [?e :time/at ?time]
                                 [?e :finance.entry/account ?account]]
                        db (:db/id account) (:time/at entry))]
        (assoc entry' :finance.entry/rank (or rank 0))))))


(defmethod entry-updates :finance.entry/posting
  [db posting]
  (s/validate schema/Posting posting)
  (when-let [errors (entry/check-posting posting nil)]
    (throw (ex-info "Semantic errors in posting"
                    {:posting posting
                     :errors errors})))
  (let [posting (update-transaction-entry db posting)
        invoice-updates (entry-updates db (:finance.posting/invoice posting))]
    (cons
      (cond-> posting
        ; if price but no cost, infer
        (and (:finance.posting/price posting)
             (nil? (:finance.posting/cost posting)))
          (assoc :finance.posting/cost
                 {:amount (:finance.posting/price posting)
                  :date (->> (:time/at posting)
                             ((juxt time/year time/month time/day))
                             (apply time/local-date))})
        ; link to invoice if items are present
        (seq invoice-updates)
          (assoc :finance.posting/invoice (:db/id (first invoice-updates))))
      invoice-updates)))


(defmethod entry-updates ::entry
  [db entry]
  (s/validate schema/JournalEntry entry)
  [(update-transaction-entry db entry)])


(derive :finance.entry/note          ::entry)
(derive :finance.entry/open-account  ::entry)
(derive :finance.entry/close-account ::entry)
(derive :finance.entry/balance-check ::entry)


(defmethod entry-updates :finance/invoice
  [db invoice]
  (s/validate schema/Invoice invoice)
  (let [item-updates (map (partial entry-updates db)
                          (:finance.invoice/items invoice))
        item-ids (map (comp :db/id first) item-updates)]
    (cons
      (assoc invoice
             :db/id (next-temp-id!)
             :finance.invoice/items (set item-ids)
             :finance.invoice/item-order (vec item-ids))
      (apply concat item-updates))))


(defmethod entry-updates :finance/item
  [db item]
  (s/validate schema/LineItem item)
  [(assoc item :db/id (next-temp-id!))])
