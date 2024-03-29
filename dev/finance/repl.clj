(ns finance.repl
  (:require
    [clojure.data :refer [diff]]
    [clojure.java.io :as io]
    [clojure.repl :refer :all]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.stacktrace :refer [print-cause-trace]]
    [clojure.string :as str]
    [clojure.tools.namespace.repl :refer [refresh]]
    [datascript.core :as ds]
    [finance.data.account :as account]
    [finance.data.balance :as balance]
    [finance.data.book :as book]
    [finance.data.budget :as budget]
    [finance.data.commodity :as commodity]
    [finance.data.core :as data]
    [finance.data.entry :as entry]
    [finance.data.invoice :as invoice]
    [finance.data.item :as item]
    [finance.data.posting :as posting]
    [finance.data.price :as price]
    [finance.data.quantity :as quantity]
    [finance.data.time :as time]
    [finance.data.transaction :as transaction]
    [finance.format.ledger :as ledger]
    [finance.format.ledger.parse :as ledger.parse]
    [finance.repl.db :as db]
    [instaparse.core :as insta]
    [puget.printer :as puget]
    [tick.alpha.api :as t]))


(defn cprint
  "Pretty-prints a colored data structure value."
  [x]
  (puget/pprint
    x
    {:width 120
     :print-color true
     :print-handlers
     {finance.data.quantity.Quantity (puget/tagged-handler 'finance/q (juxt :value :commodity))
      ,,,}}))


(defn stopwatch
  "Construct a delay which will yield the number of milliseconds between this
  function call and the time it is realized."
  []
  (let [start (System/nanoTime)]
    (delay (/ (- (System/nanoTime) start) 1e6))))


(defn human-duration
  "Converts a time duration in milliseconds to a human-friendly string."
  [elapsed]
  (cond
    (< 60000 elapsed)
    (format "%d:%04.1f" (int (/ elapsed 60000)) (mod (/ elapsed 1000) 60))

    (< 1000 elapsed)
    (format "%.3f seconds" (/ elapsed 1000))

    :else
    (format "%.3f ms" elapsed)))


(defn get-percentile
  "Selects an element that is `p` percent through the given sequence."
  [xs p]
  (let [n (count xs)]
    (cond
      (= n 1)
      (first xs)

      (<= p 0.0)
      (first xs)

      (<= 1.0 p)
      (last xs)

      :else
      (nth xs (int (* p n))))))


;; ## Parsing Tools

(defn reload-grammar!
  "Recreate the Ledger parser by loading the grammar file."
  []
  (let [parser (#'ledger.parse/load-grammar)
        p (promise)]
    (deliver p parser)
    (alter-var-root #'ledger.parse/ledger-parser (constantly p)))
  :reloaded)


(defn read-groups
  "Read the given file into groups of lines which are separated by blanks.
  Returns a lazy sequence of group strings."
  [file]
  (->> file
       (io/file)
       (io/reader)
       (line-seq)
       (ledger.parse/group-lines)))


(defn find-groups
  "Searches through the groups in a file to find ones which match the given
  pattern. Returns a sequence of indices for the matching groups."
  [file pattern]
  (keep-indexed #(when (re-seq pattern %2) %1)
                (read-groups file)))


(defn get-group
  "Reads a line group out of a file by index."
  [file index]
  (nth index (read-groups file)))


(defn debug-parse
  "Attempts to parse the given text using the current parser. Returns the
  interpreted data structure if the text parsed successfully, or nil on error.
  Prints lots of debugging information on failure or if `show?` is truthy."
  ([text]
   (debug-parse text 0 true))
  ([text index show?]
   (try
     ;; If showing this example, explicitly print input
     (when show?
       (printf "\nParsing entry %d:\n\n%s\n" index text))
     ;; Try parsing the text
     (let [parses (insta/parses @ledger.parse/ledger-parser text)]
       (cond
         ;; On failure, print out input and error message
         (insta/failure? parses)
         (do (printf "\nParsing entry %d failed:\n\n" index)
             (when-not show?
               (println text ""))
             (cprint (insta/get-failure parses))
             nil)

         ;; If parsing is ambiguous, print first two and diff
         (< 1 (count parses))
         (do (printf "\nParsing entry %d is ambiguous (%d parses):\n\n"
                     index (count parses))
             (when-not show?
               (println text ""))
             (cprint (take 2 parses))
             (println "\nDifferences:")
             (cprint (diff (first parses) (second parses)))
             nil)

         ;; Try interpreting the parse
         :else
         (do
           (when show?
             (println "Parsed:")
             (cprint (first parses)))
           (let [interpreted (ledger.parse/interpret-parse (first parses))
                 entry (first interpreted)]
             ;; If showing, explicitly print conversion:
             (when show?
               (println)
               (println "Interpreted:")
               (cprint interpreted)
               (when-let [errors (and (::data/type entry)
                                      (s/explain (::data/type entry) entry))]
                 (println)
                 (println "Validation errors:")
                 (cprint errors)))
             interpreted))))
     (catch Exception e
       (printf "\nParsing entry %d failed:\n\n" index)
       (when-not show?
         (println text ""))
       (print-cause-trace e)
       nil))))


(defn test-parser
  "Tests the parser by running it against the line groups in the given file.
  Any extra arguments will explicitly print out the results of parsing the
  groups at those indices."
  [file & show-entries]
  (let [groups (read-groups file)
        show-entry? (if (and (= 1 (count show-entries))
                             (= :all (first show-entries)))
                      (constantly true)
                      (set show-entries))
        error-limit 5
        elapsed (stopwatch)]
    (loop [entries groups
           index 0
           errors 0]
      (cond
        ;; Hit error limit
        (>= errors error-limit)
        (let [total (+ index (count entries))]
          (printf "\nStopping after %d errors at entry %d/%d (%.1f%%) in %s\n"
                  error-limit index total (* (/ index total) 100.0)
                  (human-duration @elapsed)))

        ;; Parse next entry
        (seq entries)
        (let [success? (debug-parse (first entries) index (show-entry? index))]
          (recur (rest entries)
                 (inc index)
                 (if success?
                   errors
                   (inc errors))))

        ;; Parsed everything without hitting error limit
        :else
        (do (printf "\nParsed %d entries with %d errors in %s\n"
                    index errors (human-duration @elapsed))
            (zero? errors))))))



;; ## Data Integration

;; TODO: how would I load and regularize a whole dataset?
;; - load commodity definitions
;; - load price history
;; - load all accounts, generate unique ids as needed
;; - load transaction history, generate unique ids
;;   - pull out entries
;;     - give each entry a unique id
;;     - link to tx ids
;;     - rewrite account reference to account id
;;   - pull out items, link to posting ids
;;   - group items into invoices?


(defn inspect-file
  "Inspects the parsing of a group in the given file. If no index is given, one
  is selected at random."
  ([file]
   (inspect-file file nil))
  ([file index & {:keys [book db], :or {book "repl", db @db/conn}}]
   (let [groups (read-groups file)
         index (or index (rand-int (count groups)))
         entries (debug-parse (nth groups index) index true)]
     #_
     (try
       (let [tx-updates (load/with-context book
                                           (->> entries
                                                (keep (partial load/entry-updates db))
                                                (doall)))]
         (println)
         (println "Transaction updates:")
         (doseq [tx tx-updates]
           (cprint tx)))
       (catch Exception e
         (println)
         (println "Error constructing transaction updates:")
         (print-cause-trace e))))))


#_
(defn print-stats
  "Prints out the stat maps returned by `load-ledger!` in a human-readable
  format."
  [stats]
  (letfn [(get-ps
            [nums]
            (if (= 1 (count nums))
              [(first nums) 'ms]
              (->> [0.50 0.90 1.00]
                   (map (partial get-percentile nums))
                   (map #(vector % 'ms))
                   (mapv #(vec (cons %1 %2)) [:p50 :p90 :max]))))]
    (doseq [[stat numbers] stats]
      (when-not (= "finance.import" (namespace stat))
        (if (number? numbers)
          (cprint [stat numbers])
          (cprint [stat {:count (count numbers)
                         :parse (get-ps (sort (map :parse numbers)))
                         :load  (get-ps (sort (map :load numbers)))}]))))))


#_
(defn measured-import!
  "Accepts a chunk of text to parse and import into the database. Updates the
  given stats map with a datapoint measuring the nanoseconds taken to parse and
  load the entries. Returns the updated stats map."
  [book stats [index text]]
  (try
    (let [parse-watch (stopwatch)
          entries (ledger.parse/parse-group text)
          parse-elapsed @parse-watch]
      (reduce
        (fn measure-load
          [stats entry]
          (let [type-key (load/entry-dispatch nil entry)
                load-watch (stopwatch)]
            (load/load-entry! db/conn book entry)
            (update stats type-key
                    (fnil conj [])
                    {:parse parse-elapsed
                     :load @load-watch})))
        stats
        entries))
    (catch Exception ex
      (println (puget.color.ansi/sgr (format "ERROR parsing entry %d: (%s) %s"
                                             index (.getClass ex) (.getMessage ex))
                                     :red))
      (println text)
      (some-> ex ex-data (dissoc :schema) cprint)
      (println)
      (print-cause-trace ex)
      (update stats :errors (fnil inc 0)))))


#_
(defn load-ledger!
  "Parses, interprets, and loads all entries in `file` into the database in
  `db/conn`. Prints out the number of each entity loaded and the total time
  elapsed."
  [book file & {:as opts}]
  (let [group-entries (if (:price-db opts)
                        (partial map #(str % "\n"))
                        ledger.parse/group-lines)]
    (->>
      file
      (io/file)
      (io/reader)
      (line-seq)
      (group-entries)
      (map vector (range))
      (reduce (partial measured-import! book) (sorted-map)))))


#_
(defn load-commodities!
  [ledger-root]
  (println "Loading commodity definitions...")
  (let [path (str ledger-root "/commodities.ledger")]
    (time (print-stats (load-ledger! nil path)))))


#_
(defn load-prices!
  [ledger-root]
  (println "Loading price history...")
  (let [path (str ledger-root "/prices.dat")]
    (time (print-stats (load-ledger! nil path :price-db true)))))


#_
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


#_
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
