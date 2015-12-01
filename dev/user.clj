(ns user
  (:require
    [blocks.core :as block]
    [clojure.data :refer [diff]]
    [clojure.java.io :as io]
    [clojure.repl :refer :all]
    [clojure.stacktrace :refer [print-cause-trace]]
    [clojure.string :as str]
    [clojure.tools.namespace.repl :refer [refresh]]
    [instaparse.core :as parse]
    (merkledag
      [core :as merkle]
      [graph :as graph])
    [merkledag.data.finance.core :as finance]
    [puget.printer :as puget]))


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


(defn try-parsing
  "Attempts to parse the given text using the current parser. Returns true if
  the text parsed successfully, false on error."
  ([text]
   (try-parsing text 0 true))
  ([text index show?]
   (try
     ; If showing this example, explicitly print input
     (when show?
       (printf "\nParsing entry %d:\n\n%s\n" index text))
     ; Try parsing the text
     (let [parsed (finance/ledger-parser text)]
       (if (parse/failure? parsed)
         ; On failure, print out input and error message
         (do (printf "\nParsing entry %d failed:\n\n" index)
             (when-not show? (println text ""))
             (puget/cprint parsed)
             false)
         ; Calculate all possible parses
         (let [parses (parse/parses finance/ledger-parser text)]
           (if (< 1 (count parses))
             ; If parsing is ambiguous, print first two and diff
             (do (printf "\nParsing entry %d is ambiguous (%d parses):\n\n"
                         index (count parses))
                 (when-not show? (println text ""))
                 (puget/cprint (take 2 parses))
                 (println "\nDifferences:")
                 (puget/cprint (diff (first parses) (second parses)))
                 false)
             ; Try interpreting the parse
             (let [interpreted (finance/interpret-parse parsed)]
               ; If showing, explicitly print conversion:
               (when show?
                 (println "Parsed:")
                 (puget/cprint parsed)
                 (println)
                 (println "Interpreted:")
                 (puget/cprint interpreted))
                 true)))))
     (catch Exception e
       (printf "\nParsing entry %d failed:\n\n" index)
       (when-not show? (println text ""))
       (print-cause-trace e)
       false))))


(defn test-parser
  "Tests the parser by running it against the line groups in the given file.
  Any extra arguments will explicitly print out the results of parsing the
  groups at those indices."
  [file & show-entries]
  (let [groups (-> file io/file io/reader line-seq finance/group-lines)
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
          (printf "\nStopping after %d errors at entry %d in %s\n"
                  error-limit index (human-duration (get-elapsed)))

        ; Parse next entry
        (seq entries)
          (let [success? (try-parsing (first entries) index (show-entries index))]
            (recur (rest entries) (inc index) (if success? errors (inc errors))))

        ; Parsed everything without hitting error limit
        :else
          (do (printf "\nParsed %d entries with %d errors in %s\n"
                      index errors (human-duration (get-elapsed)))
              (zero? errors))))))


(defn reload-grammar!
  "Recreate the Ledger parser by loading the grammar file."
  []
  (alter-var-root #'finance/ledger-parser (constantly (parse/parser (io/resource "grammar/ledger.bnf"))))
  :reloaded)
