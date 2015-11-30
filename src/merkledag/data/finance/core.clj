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
    {:CommodityCode (fn [code] [:CommodityCode (if (= "$" code) 'USD (symbol code))])
     :Quantity (fn [& children]
                 (let [cfg (into {} children)]
                 (tagged-literal 'finance/$
                                 [(:Number cfg)
                                  (:CommodityCode cfg)])))
     :Number (fn [& digits] [:Number (BigDecimal. (str/join digits))])
     :AccountPathSegment (fn [& words] (str/join words))
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
       (map (comp interpret-parse ledger-parser))))


(defn parse-files
  [& files]
  (loop [fs (seq files)
         data {}]
    (if-let [f (first fs)]
      (do (println "Parsing file" f)
          (printf "  (%d lines)\n" (count (line-seq (io/reader (io/file f))))))
      data)))
