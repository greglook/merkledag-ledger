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
      [import :as fimport]
      [parse :as parse]
      [schema :as schema]
      [transaction :as transaction])
    [puget.printer :as puget]
    [schema.core :as s]
    [user.db :as db]))


;; ## Debugging Utilities

(defn cprint
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


(defn reload-grammar!
  "Recreate the Ledger parser by loading the grammar file."
  []
  (alter-var-root #'parse/ledger-parser (constantly (insta/parser (io/resource "grammar/ledger.bnf"))))
  :reloaded)



;; ## Parsing Tools

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
       (let [tx-updates (fimport/with-tx-context
                          (->> entries
                               (keep (partial fimport/entry-updates db book))
                               (doall)))]
         (println)
         (println "Transaction updates:")
         (doseq [tx tx-updates] (cprint tx)))
       (catch Exception e
         (println)
         (println "Error constructing transaction updates:")
         (print-cause-trace e))))))



;; ## Data Integration

(defn load-entry!
  "Loads the parsed and interpreted entry into the database in `db/conn`.
  Throws an exception if generating or transacting the updates fails."
  [book entry]
  (try
    (fimport/with-tx-context
      (when-let [tx-updates (->> entry
                                 (fimport/entry-updates @db/conn book)
                                 (remove nil?)
                                 (seq))]
        (d/transact! db/conn tx-updates)))
    (catch Exception ex
      (println "Error loading entry!")
      (cprint (ex-data ex))
      (throw ex))))


(defn load-file!
  "Parses, interprets, and loads all entries in `file` into the database
  in `db/conn`."
  [book file]
  (reduce
    (fn [stats entry]
      (let [type-key (fimport/import-dispatch nil book entry)]
        (load-entry! book entry)
        (update stats type-key (fnil inc 0))))
    (sorted-map)
    (parse/parse-file file)))
