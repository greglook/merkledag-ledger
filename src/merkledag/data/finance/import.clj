(ns merkledag.data.finance.import
  "Functions to integrate incoming (and incomplete) data into the financial
  database. Import takes a _raw entry_ and the current database and returns a
  set of transactions to import the data represented by the entry."
  (:require
    (clj-time
      [coerce :as ctime]
      [core :as time])
    [clojure.string :as str]
    [datascript.core :as d]))


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

(def ^:dynamic *book-name*
  "String naming the current set of books being imported."
  nil)


(defn import-dispatch
  "Selects an integration dispatch value based on the argument type."
  [db entry]
  (if (vector? entry)
    (keyword "finance.import" (name (first entry)))
    (:data/type entry)))


(defmulti entry-updates
  "Generates and returns a sequence of datums which can be transacted onto the
  database to integrate the given entry."
  #'import-dispatch)


(defmethod entry-updates :default
  [db entry]
  (let [entry-type (import-dispatch db entry)]
    (throw (ex-info (str "Unsupported entry type: " entry-type)
                    {:type entry-type
                     :entry entry}))))


(defmethod entry-updates ::ignored
  [db header]
  ; Ignored
  nil)


(derive :finance.import/CommentHeader ::ignored)
(derive :finance.import/CommentBlock  ::ignored)
(derive :finance.import/IncludeFile   ::ignored)


(defmethod entry-updates :finance/commodity
  [db commodity]
  (let [code (:finance.commodity/code commodity)
        entity (when db (d/entity db [:finance.commodity/code code]))]
    [(-> commodity
         (dissoc :data/sources ::format ::options)
         (assoc :db/id (:db/id entity -1))
         (cond->
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
  (let [path (:finance.account/path account)
        [extant] (d/q '[:find [?a]
                        :in $ ?books ?path
                        :where [?a :finance.account/path ?path]
                               [?a :finance.account/book ?books]
                               [?a :data/type :finance/account]]
                      db *book-name* path)]
    [(-> account
         (assoc :db/id (or extant -1)
                :finance.account/book *book-name*
                :finance.account/path path)
         (dissoc :data/sources))]))


(defmethod entry-updates :finance/transaction
  [db transaction]
  (when-not *book-name*
    (throw (RuntimeException. "Must bind *book-name* to integrate transactions!")))
  ; TODO: implement
  [])
