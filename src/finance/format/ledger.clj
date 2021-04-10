(ns finance.format.ledger
  "Code for working with text-based ledger files."
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [finance.format.ledger.parse :as parse]
    [finance.format.ledger.print :as print]))


;; TODO: implement standard format API



(defn parse-string
  "Parse a string of ledger text into a vector of entity maps."
  [text]
  (->> text
       (str/split-lines)
       (parse/parse-lines)
       (vec)))


(defn parse
  "Parse the source, returning a lazy sequence of interpreted ledger entries.
  Source may be anything coercible to a reader."
  [source]
  (->> source
       (io/reader)
       (line-seq)
       (parse/parse-lines)))
