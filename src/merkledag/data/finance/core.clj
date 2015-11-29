(ns merkledag.data.finance.core
  "..."
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))


(defn group-lines
  "Takes a sequence of lines and returns a new sequence of groups of lines
  which were separated by blank lines in the input."
  [lines]
  (->> lines
       (partition-by #{""})
       (remove #(every? #{""} %))))


(defn run-parser
  [parser initial lines]
  (loop [state :start
         value initial
         lines (seq lines)]
    (if-let [line (first lines)]
      (if-let [[match handler]
               (some (fn [[pattern handler]]
                       (when-let [match (re-matches pattern line)]
                         [match handler]))
                     (get parser state))]
        (let [[next-state next-value] (handler value match)]
          (recur next-state next-value (next lines)))
        (assoc value
          :tag :error
          :error/last-tag (:tag value)
          :error/lines (vec lines)
          :error/message
          (format "Input line did not match any patterns in state %s: %s"
                  state (pr-str line))))
      value)))


(defn- assoc-match
  ([next-state k]
   (assoc-match next-state k identity))
  ([next-state k val-fn]
   (fn [value [line match]]
     [next-state (assoc value k (val-fn match))])))


(defn parse-commodity
  [lines]
  (run-parser
    {:start   {#"commodity (\S+)"
               (assoc-match :options :commodity/code)}
     :options {#"    note (.+)"
               (assoc-match :options :commodity/note)
               #"    format (.+)"
               (assoc-match :options :commodity/format)
               #"    nomarket"
               (assoc-match :options :commodity/nomarket boolean)
               #"    default"
               (assoc-match :options :commodity/default boolean)}}
    {:tag :commodity}
    lines))


(defn parse-account
  [lines]
  (run-parser
    {:start   {#"; (.+)"
               (fn [value [line match]]
                 [:start (update value :account/comments (fnil conj []) match)])
               #"account (.+)"
               (assoc-match :options :account/path)}
     :options {#"    note (.+)"
               (assoc-match :options :account/note)
               #"    alias (\S+)"
               (assoc-match :options :account/alias)
               #"    assert (.+)"
               (assoc-match :options :account/assert)}}
    {:tag :account}
    lines))


(defn parse-group
  "Figures out what a group of lines represents. Emits a map with a `:tag` key
  and properties parsed from each group."
  [lines]
  (cond
    (and (= 1 (count lines))
         (.startsWith ";;;" (first lines)))
      {:tag :comment-header, :comment/header (str/replace (first lines) #"\s*;+\s*" "")}
    (every? #(.startsWith % ";") lines)
      {:tag :comment-block, :comment/text (->> lines
                                               (map #(if (= ";" %)
                                                       "\n\n"
                                                       (str/replace % #"^;+\s+" "")))
                                               (str/join " "))}
    (and (= 1 (count lines))
         (.startsWith (first lines) "C "))
      (let [[line from-val from-commodity to-val to-commodity]
            (re-matches #"C (\d+\.\d+) (\S+) = (\d+\.\d+) (\S+)" (first lines))]
        {:tag :commodity-conversion
         :conversion/from (tagged-literal 'finance/$ [(BigDecimal. from-val) (symbol from-commodity)])
         :conversion/to (tagged-literal 'finance/$ [(BigDecimal. to-val) (symbol to-commodity)])})
    (and (= 1 (count lines))
         (.startsWith (first lines) "P "))
      (let [[line date commodity price]
            (re-matches #"P (\S+) (\S+)\s+\$(\d+\.\d+)" (first lines))]
        {:tag :price
         :price/commodity commodity
         :price/cost (tagged-literal 'finance/$ [(BigDecimal. price) 'USD])})
    (some (partial re-seq #"^commodity ") lines)
      (parse-commodity lines)
    (some (partial re-seq #"^account ") lines)
      (parse-account lines)
    :else
      {:tag :unknown, :unknown/lines (vec lines)}))


(defn parse-file
  [data file]
  (->> (io/file file)
       (io/reader)
       (line-seq)
       (group-lines)
       (map parse-group)))


(defn parse-files
  [& files]
  (loop [fs (seq files)
         data {}]
    (if-let [f (first fs)]
      (do (println "Parsing file" f)
          (printf "  (%d lines)\n" (count (line-seq (io/reader (io/file f))))))
      data)))
