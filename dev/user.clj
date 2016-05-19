(ns user
  (:require
    [blocks.core :as block]
    (clj-time
      [coerce :as ctime]
      [core :as time]
      [format :as ftime])
    [clojure.data :refer [diff]]
    [clojure.java.io :as io]
    [clojure.repl :refer :all]
    [clojure.stacktrace :refer [print-cause-trace]]
    [clojure.string :as str]
    [clojure.tools.namespace.repl :refer [refresh]]
    [datascript.core :as d]
    [instaparse.core :as insta]
    [merkledag.core :as merkle]
    [merkledag.data.finance :as finance]
    (merkledag.data.finance
      [account :as account]
      [load :as load]
      [schema :as schema]
      [transaction :as transaction])
    [merkledag.data.finance.ledger.parse :as parse]
    [puget.printer :as puget]
    [schema.core :as s]
    [user.db :as db]))


(defn cprint
  "Pretty-prints a colored data structure value."
  [x]
  (puget/pprint
    x
    {:width 120
     :print-color true
     :print-handlers
     {java.util.UUID (puget/tagged-handler 'uuid str)
      merkledag.data.finance.types.Quantity (puget/tagged-handler 'finance/$ (juxt :value :commodity))
      org.joda.time.DateTime (puget/tagged-handler 'inst str)
      org.joda.time.LocalDate (puget/tagged-handler 'time/date str)
      org.joda.time.Interval (puget/tagged-handler 'time/interval #(vector (time/start %) (time/end %)))
      schema.utils.ValidationError (puget/tagged-handler 'schema/error schema.utils/validation-error-explain)}}))


(defn human-duration
  "Converts a time duration in milliseconds to a human-friendly string."
  [elapsed]
  (cond
    (> elapsed 60000)
      (format "%d:%04.1f" (int (/ elapsed 60000)) (mod (/ elapsed 1000) 60))
    (> elapsed 1000)
      (format "%.3f seconds" (/ elapsed 1000))
    :else
      (format "%.3f ms" elapsed)))


(defn nanos->ms
  "Converts an integer duration in nanoseconds to a vector with a floating-point
  millisecond time and the symbol `ms`."
  [nanos]
  [(/ nanos 1000000.0) 'ms])


(defn get-percentile
  "Selects an element that is `p` percent through the given sequence."
  [xs p]
  (let [n (count xs)]
    (cond
      (= n 1)
        (first xs)
      (<= p 0.0)
        (first xs)
      (>= p 1.0)
        (last xs)
      :else
        (nth xs (int (* p n))))))



;; ## Parsing Tools

(defn reload-grammar!
  "Recreate the Ledger parser by loading the grammar file."
  []
  (alter-var-root #'parse/ledger-parser (constantly (insta/parser (io/resource "grammar/ledger.bnf"))))
  :reloaded)


(defn find-groups
  "Searches through the groups in a file to find ones which match the given
  pattern. Returns a sequence of indices for the matching groups."
  [file pattern]
  (->> file io/file io/reader line-seq parse/group-lines
       (keep-indexed #(when (re-seq pattern %2) %1))))


(defn get-group
  "Reads a line group out of a file by index."
  [file index]
  (-> file io/file io/reader line-seq parse/group-lines (nth index)))


(defn debug-parse
  "Attempts to parse the given text using the current parser. Returns the
  interpreted data structure if the text parsed successfully, or nil on error.
  Prints lots of debugging information on failure or if `show?` is truthy."
  ([text]
   (debug-parse text 0 true))
  ([text index show?]
   (try
     ; If showing this example, explicitly print input
     (when show?
       (printf "\nParsing entry %d:\n\n%s\n" index text))
     ; Try parsing the text
     (let [parses (insta/parses parse/ledger-parser text)]
       (cond
         ; On failure, print out input and error message
         (insta/failure? parses)
           (do (printf "\nParsing entry %d failed:\n\n" index)
               (when-not show? (println text ""))
               (cprint (insta/get-failure parses))
               nil)

         ; If parsing is ambiguous, print first two and diff
         (< 1 (count parses))
           (do (printf "\nParsing entry %d is ambiguous (%d parses):\n\n"
                       index (count parses))
               (when-not show? (println text ""))
               (cprint (take 2 parses))
               (println "\nDifferences:")
               (cprint (diff (first parses) (second parses)))
               nil)

         ; Try interpreting the parse
         :else
           (do
             (when show?
               (println "Parsed:")
               (cprint (first parses)))
             (let [interpreted (parse/interpret-parse (first parses))
                   entry (first interpreted)]
               ; If showing, explicitly print conversion:
               (when show?
                 (println)
                 (println "Interpreted:")
                 (cprint interpreted)
                 (when-let [errors (some->
                                     {:finance/account schema/AccountDefinition
                                      :finance/commodity schema/CommodityDefinition
                                      :finance/price schema/CommodityPrice
                                      :finance/transaction schema/Transaction}
                                     (get (:data/type entry))
                                     (s/check entry))]
                   (println)
                   (println "Validation errors:")
                   (cprint errors)))
               interpreted))))
     (catch Exception e
       (printf "\nParsing entry %d failed:\n\n" index)
       (when-not show? (println text ""))
       (print-cause-trace e)
       nil))))


(defn test-parser
  "Tests the parser by running it against the line groups in the given file.
  Any extra arguments will explicitly print out the results of parsing the
  groups at those indices."
  [file & show-entries]
  (let [groups (-> file io/file io/reader line-seq parse/group-lines)
        show-entries (set show-entries)
        error-limit 5
        start (System/nanoTime)
        get-elapsed #(/ (- (System/nanoTime) start) 1000000.0)]
    (loop [entries groups
           index 0
           errors 0]
      (cond
        ; Hit error limit
        (>= errors error-limit)
          (let [total (+ index (count entries))]
            (printf "\nStopping after %d errors at entry %d/%d (%.1f%%) in %s\n"
                    error-limit index total (* (/ index total) 100.0)
                    (human-duration (get-elapsed))))

        ; Parse next entry
        (seq entries)
          (let [success? (debug-parse (first entries) index (show-entries index))]
            (recur (rest entries) (inc index) (if success? errors (inc errors))))

        ; Parsed everything without hitting error limit
        :else
          (do (printf "\nParsed %d entries with %d errors in %s\n"
                      index errors (human-duration (get-elapsed)))
              (zero? errors))))))



;; ## Data Integration

(defn inspect-file
  "Inspects the parsing of a group in the given file. If no index is given, one
  is selected at random."
  ([file]
   (inspect-file file nil))
  ([file index & {:keys [book db], :or {book "repl", db @db/conn}}]
   (let [groups (-> file io/file io/reader line-seq parse/group-lines)
         index (or index (rand-int (count groups)))
         entries (debug-parse (nth groups index) index true)]
     (try
       (let [tx-updates (load/with-context book
                          (->> entries
                               (keep (partial load/entry-updates db))
                               (doall)))]
         (println)
         (println "Transaction updates:")
         (doseq [tx tx-updates] (cprint tx)))
       (catch Exception e
         (println)
         (println "Error constructing transaction updates:")
         (print-cause-trace e))))))


(defn print-stats
  "Prints out the stat maps returned by `load-ledger!` in a human-readable
  format."
  [stats]
  (let [get-ps
        (fn [nums]
          (if (= 1 (count nums))
            (nanos->ms (first nums))
            (->> [0.50 0.90 1.00]
                 (map (partial get-percentile nums))
                 (map nanos->ms)
                 (mapv #(vec (cons %1 %2)) [:p50 :p90 :max]))))]
    (doseq [[stat numbers] stats]
      (when-not (= "finance.import" (namespace stat))
        (if (number? numbers)
          (cprint [stat numbers])
          (cprint [stat {:count (count numbers)
                         :parse (get-ps (sort (map :parse numbers)))
                         :load  (get-ps (sort (map :load numbers)))}]))))))


(defn measured-import!
  "Accepts a chunk of text to parse and import into the database. Updates the
  given stats map with a datapoint measuring the nanoseconds taken to parse and
  load the entries. Returns the updated stats map."
  [book stats text]
  (try
    (let [parse-start (System/nanoTime)
          entries (parse/parse-group text)
          parse-elapsed (- (System/nanoTime) parse-start)]
      (reduce
        (fn measure-load
          [stats entry]
          (let [type-key (load/entry-dispatch nil entry)
                load-start (System/nanoTime)]
            (load/load-entry! db/conn book entry)
            (update stats type-key
                    (fnil conj [])
                    {:parse parse-elapsed
                     :load (- (System/nanoTime) load-start)})))
        stats
        entries))
    (catch Exception ex
      (println (puget.color.ansi/sgr (str "ERROR: " (.getMessage ex)) :red))
      (println text)
      (some-> ex ex-data (dissoc :schema) cprint)
      (println)
      (update stats :errors (fnil inc 0)))))


(defn load-ledger!
  "Parses, interprets, and loads all entries in `file` into the database in
  `db/conn`. Prints out the number of each entity loaded and the total time
  elapsed."
  [book file & {:as opts}]
  (let [group-entries (if (:price-db opts)
                        (partial map #(str % "\n"))
                        parse/group-lines)]
    (->>
      file
      (io/file)
      (io/reader)
      (line-seq)
      (group-entries)
      (reduce (partial measured-import! book) (sorted-map)))))


(defn load-commodities!
  [ledger-root]
  (println "Loading commodity definitions...")
  (let [path (str ledger-root "/commodities.ledger")]
    (time (print-stats (load-ledger! nil path)))))


(defn load-prices!
  [ledger-root]
  (println "Loading price history...")
  (let [path (str ledger-root "/prices.dat")]
    (time (print-stats (load-ledger! nil path :price-db true)))))


(defn load-books!
  [ledger-root book]
  (let [book-dir (io/file ledger-root "books" book)
        book-files (list* (io/file book-dir "accounts.ledger")
                          (io/file book-dir "books.ledger")
                          (.listFiles (io/file book-dir "journals")))]
    (time
      (doseq [file book-files]
        (let [relpath (subs (str file) (inc (count (str book-dir))))]
          (println "Loading" book "book file" relpath "..."))
        (time (print-stats (load-ledger! book file)))
        (println)))))


(defn load-all!
  [ledger-root]
  (time
    (do (load-commodities! ledger-root)
        (println)
        (load-prices! ledger-root)
        (println)
        (doseq [book (map #(.getName %) (.listFiles (io/file ledger-root "books")))]
          (load-books! ledger-root book)
          (println)))))
