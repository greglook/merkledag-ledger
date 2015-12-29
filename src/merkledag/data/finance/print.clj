(ns merkledag.data.finance.print
  "Ledger file printing code."
  (:require
    (clj-time
      [coerce :as ctime]
      [core :as time]
      [format :as ftime])
    [clojure.java.io :as io]
    [clojure.string :as str]))


;; ## Rendering Functions

(defn render-dispatch
  "Selects a rendering key based on the argument type."
  [entry]
  (if (vector? entry)
    (first entry)
    (:data/type entry)))


(defmulti render-entry
  "Renders the given ledger entry as a string."
  #'render-dispatch)


(defn render-file
  "Renders a ledger file from a sequence of entries. Returns a string of the
  file contents."
  [entries]
  (str/join "\n\n" (map render-entry entries)))



;; ## Entry Rendering Methods

(defn render-amount
  "Renders a financial quantity."
  [quantity]
  (case (:commodity quantity)
    USD
      (format "$%.2f" (:value quantity))
    (format "%.3f %s" (:value quantity) (:commodity quantity))))


(defn render-posting
  [posting]
  (let [account-name (if (vector? (:account posting))
                       (str/join ":" (:account posting))
                       (name (:account posting)))
        unused (dissoc posting :account)]
    (str
      (if-let [amount (:amount posting)]
        (format "    %-45s %20s" account-name (render-amount amount))
        (format "    %s" account-name))
      (when-let [time (:time posting)]
        (str "\n        ; time: " time))
      (when-let [sources (seq (:sources posting))]
        (str/join (map #(str "\n        ; source: " (name (:source %)) "|" (:line %)) sources)))
      (when-let [unused (not-empty (dissoc posting :account :amount :time :sources))]
        (str "\n        ; unused: " (pr-str unused))))))


(defn render-transaction
  [tx]
  (str (when-let [unused (not-empty (dissoc tx :parse/source :date :title :status :time :postings))]
         (str "; " (pr-str unused) "\n"))
       (:date tx)
       (case (:status tx) :cleared " *" :pending " !" nil)
       (when-let [code (:code tx)]
         (str " (" code ")"))
       " " (:title tx) "\n"
       (when-let [time (:time tx)]
         (str "    ; time: " time "\n"))
       ; meta
       ; comments
       (when-let [postings (seq (:postings tx))]
         (str/join "\n" (map render-posting postings)))))


(defmethod render-entry :default
  [entry]
  (str "; " (pr-str entry)))


(defmethod render-entry :CommentBlock
  [[_ text]]
  (str "; " text))


(defmethod render-entry :Transaction
  [[_ tx]]
  (render-transaction tx))
