(ns user
  (:require
    [blocks.core :as block]
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


(defn reload-grammar!
  []
  (alter-var-root #'finance/ledger-parser (constantly (parse/parser (io/resource "grammar/ledger.bnf"))))
  :reloaded)


(defn test-parser
  [file & indexes]
  (let [groups (-> file io/file io/reader line-seq finance/group-lines)
        indexes (set indexes)
        error-limit 5
        start (System/nanoTime)]
    (loop [i 0
           entries groups
           errors 0]
      (if (>= errors error-limit)
        (printf "\nStopping after %d errors at entry %d in %.3f ms\n"
                error-limit i (/ (- (System/nanoTime) start) 1000000.0))
        (if-let [entry (first entries)]
          (do
            ; If one of the given indexes, explicitly print input:
            (when (contains? indexes i)
              (printf "\nParsing entry %d:\n\n%s\n" i entry))
            (let [parsed (finance/ledger-parser entry)]
              ; Try parsing - if failure, print message:
              (if (parse/failure? parsed)
                (do (printf "\nParsing entry %d failed:\n" i)
                    (when-not (contains? indexes i) (println entry))
                    (puget/cprint parsed)
                    (recur (inc i) (rest entries) (inc errors)))
                ; Count number of parses - if ambiguous, print first 5:
                (let [parses (parse/parses finance/ledger-parser entry)]
                  (if (< 1 (count parses))
                    (do (printf "\nParsing entry %d is ambiguous (%d parses):\n"
                                i (count parses))
                        (when-not (contains? indexes i) (println entry))
                        (println)
                        (puget/cprint (take 5 parses))
                        (recur (inc i) (rest entries) (inc errors)))
                    ; Try interpreting the parse
                    (if (try
                          (let [interpreted (finance/interpret-parse parsed)]
                            ; If one of the given indexes, explicitly print conversion:
                            (when (contains? indexes i)
                              (println "Parsed:")
                              (puget/cprint parsed)
                              (println)
                              (println "Interpreted:")
                              (puget/cprint interpreted))
                            true)
                          (catch Exception e
                            (printf "\nInterpreting entry %d failed:\n\n" i)
                            (when-not (contains? indexes i) (println entry))
                            (puget/cprint parsed)
                            (print-cause-trace e)
                            false))
                      (recur (inc i) (rest entries) errors)
                      (recur (inc i) (rest entries) (inc errors))))))))
          ; Parsed everything without hitting error limit.
          (do (printf "\nParsed %d entries in %.3f ms\n"
                      i (/ (- (System/nanoTime) start) 1000000.0))
              (zero? errors)))))))
