(ns merkledag.data.finance.core
  "..."
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [instaparse.core :as insta]))



(def ledger-parser
  (insta/parser (io/resource "grammar/ledger.bnf")))


(defn interpret-parse
  [tree]
  (insta/transform
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


(defn parse-file
  [data file]
  (->> (io/file file)
       (slurp)
       (ledger-parser)
       (interpret-parse)))


(defn parse-files
  [& files]
  (loop [fs (seq files)
         data {}]
    (if-let [f (first fs)]
      (do (println "Parsing file" f)
          (printf "  (%d lines)\n" (count (line-seq (io/reader (io/file f))))))
      data)))
