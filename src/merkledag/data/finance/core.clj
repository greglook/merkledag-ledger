(ns merkledag.data.finance.core
  "..."
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [instaparse.core :as parse]))



(def ledger-parser
  (parse/parser (io/resource "grammar/ledger.bnf")))


(defn parse-input
  [input]
  (->> (ledger-parser input)
       #_
       (walk/postwalk
         (fn [v]
           (if (vector? v)
             (case (first v)
               :INT (Integer/parseInt (str/join (rest v)))
               :PLUS '+
               :MINUS '-
               :MULT '*
               :DIV '/
               v)
             v)))))



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
          (if next-value
            (recur next-state next-value (next lines))
            (recur next-state value lines)))
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


(defn- tack-on
  [value k v]
  (update value k (fnil conj []) v))


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
                 [:start (tack-on value :account/comments match)])
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


(defn parse-tx
  [lines]
  (run-parser
    {:start   {#"; (.+)"
               (fn [tx [line match]]
                 [:start (tack-on tx :tx/sources match)])
               #"(\d{4}-\d\d-\d\d) ?([*!]?) (.+)"
               (fn [tx [line date status memo]]
                 [:meta (assoc tx
                               :tx/date date
                               :tx/status (case status "*" :cleared "!" :pending nil)
                               :tx/memo memo)])}
     :meta [[#"    ; ([a-zA-Z-]+): (.+)"
             (fn [tx [line tag value]]
               [:meta (update tx :tx/meta assoc (keyword tag) value)])]
            [#"    ; :([a-zA-Z-]+):"
             (fn [tx [line tag]]
               [:meta (update tx :tx/meta assoc (keyword tag) true)])]
            [#"    ; (.+)"
             (fn [tx [line match]]
               [:meta (tack-on tx :tx/comments match)])]
            [#"    [a-zA-Z].+"
             (constantly [:posting])]]
     :posting [[#"    ([a-zA-Z0-9-]+(?::\w+|:\w+ \w+)*)"
                (fn [tx [line account]]
                  [:posting (tack-on tx :tx/postings {:posting/account account})])]
               [#"    ([a-zA-Z0-9-]+(?::\w+|:\w+ \w+)*)  \s*(-?\$[0-9,]+\.\d+|[A-Z]+ -?[0-9,]+\.\d+|-?[0-9,]+\.\d+ [a-zA-Z0-9_]+)"
                (fn [tx [line account value]]
                  [:posting (tack-on tx :tx/postings {:posting/account account, :posting/amount value})])]
               [#"        ; ([a-zA-Z-]+): (.+)"
                (fn [tx [line tag value]]
                  (let [posting (last (:tx/postings tx))
                        posting' (tack-on posting :posting/meta [(keyword tag) value])
                        postings (vec (concat (butlast (:tx/postings tx)) [posting']))]
                    [:posting (assoc tx :tx/postings postings)]))]
               [#"        ; :([a-zA-Z-]+):"
                (fn [tx [line tag]]
                  (let [posting (last (:tx/postings tx))
                        posting' (tack-on posting :posting/meta [(keyword tag) true])
                        postings (vec (concat (butlast (:tx/postings tx)) [posting']))]
                    [:posting (assoc tx :tx/postings postings)]))]
               [#"        ; (.+)"
                (fn [tx [line match]]
                  (let [posting (last (:tx/postings tx))
                        posting' (tack-on posting :posting/comments match)
                        postings (vec (concat (butlast (:tx/postings tx)) [posting']))]
                    [:posting (assoc tx :tx/postings postings)]))]]
     }
    {:tag :tx}
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
    (some (partial re-seq #"^\d{4}-\d\d-\d\d ") lines)
      (parse-tx lines)
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
