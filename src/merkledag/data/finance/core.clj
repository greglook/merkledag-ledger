(ns merkledag.data.finance.core
  "..."
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [instaparse.core :as parse]))


(def ledger-parser
  (parse/parser (io/resource "grammar/ledger.bnf")))


(defn interpret-parse
  [tree]
  (parse/transform
    {:CommodityCode (fn [code] [:commodity/code (if (= "$" code) 'USD (symbol code))])
     :Number (fn [& digits] [:number (BigDecimal. (str/join digits))])
     :Percentage (fn [number] [:% (/ (second number) 100)])
     :Quantity (fn [& children]
                 (when (not= '("0") children)
                   (let [cfg (into {} children)]
                     (tagged-literal 'finance/$
                                     [(:number cfg)
                                      (:commodity/code cfg)]))))
     :AccountPathSegment (fn [& words] (str/join words))
     :TxMeta (fn ([k] [:tx/meta (keyword k) true])
                 ([k v] [:tx/meta (keyword k) v]))
     :PostingMeta (fn ([k] [:posting/meta (keyword k) true])
                      ([k v] [:posting/meta (keyword k) v]))
    }
    tree))


(defn group-lines
  "Takes a sequence of lines and returns a new sequence of groups of lines
  which were separated by blank lines in the input."
  [lines]
  (->> lines
       (partition-by #{""})
       (remove #(every? #{""} %))
       (map #(str (str/join "\n" %) "\n"))))


(defn parse-file
  [data file]
  (->> (io/file file)
       (io/reader)
       (line-seq)
       (group-lines)
       (mapcat (comp interpret-parse ledger-parser))))


(defn parse-files
  [& files]
  (loop [fs (seq files)
         data {}]
    (if-let [f (first fs)]
      (do (println "Parsing file" f)
          (printf "  (%d lines)\n" (count (line-seq (io/reader (io/file f))))))
      data)))
