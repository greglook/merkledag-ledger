(ns merkledag.data.finance.core
  "..."
  (:require
    [blocks.core :as block]
    [clj-time.core :as time]
    [merkledag.core :as merkle]
    [merkledag.link :as link]))


(defn ->finance-data
  [graph root]
  {:graph graph
   :root (atom root)})


(def update-node
  "Updates the given node by substituting in the given links and potentially
  running a function to update the body.

  If the given links have names, and links with matching names exist in the
  current node, they will be replaced with the new links. Otherwise, the links
  will be appended "
  ([node links]
   (update-node node links identity))
  ([node links f & args]
   (let [links' (reduce (fn [ls l]
                          (let [[before after] (split-with #(not= (:name l) (:name %)) ls)]
                            (concat before [l] (rest after))))
                        (:links node)
                        links)]
     (merkle/node links' (apply f (:data node) args)))))


(def update-node-in
  "Returns a sequence of nodes, the first of which is the updated root node."
  [node path f & args]
  (if (empty? path)
    ; Base Case: empty path segment
    [(apply f node args)]
    ; Recursive Case: first path segment
    (let [link-name (str (first path))
          child (when node
                  (some-> (link/resolve link-name (:links node))
                          (deref)))]
      (when-let [children (apply update-node-in child (rest path) f args)]
        (cons (if node
                (update-node node [(merkle/link link-name (first children))])
                (merkle/node [(merkle/link link-name (first children))] nil))
              children)))))


(defn year-seek
  "Returns a lazy sequence of entries from the given root node.

  With no extra arguments, this will return a lazy sequence of each entry in
  ascending order, starting from the earliest entry. If `:reverse` is set, the
  sequence will be descending from the latest entry.

  If `:marker` and `:time-key` are set, then the sequence will begin iterating
  at the earliest entry which falls after the marker. The time-key is used as a
  function to get the instant from each entry. If reversed, then the sequence
  will begin at the latest entry which falls before the marker.

  The node is expected to have a set of links named by year, pointing to a node
  with a vector of elements sorted ascending by time (earliest first)."
  [history opts]
  (let [{:keys [time-key marker]} opts
        reverse? (:reverse opts)]
    (when (or (and marker (nil? time-key))
              (and time-key (nil? marker)))
      (throw (IllegalArgumentException.
               "The time-key and marker options must both be set.")))
    (-> (:links history)
        (->>
          (filter #(re-matches #"\d+" (:name %)))
          (sort-by :name))
        (cond->>
          reverse? (reverse)
          marker (drop-while (fn [link]
                               (let [link-year (Integer/parseInt (:name link))
                                     marker-year (time/year marker)]
                                 (if reverse?
                                   (< marker-year link-year)
                                   (> marker-year link-year))))))
        (->>
          (mapcat (fn [link]
                    (cond->> (:data (deref link))
                      time-key (sort-by time-key)
                      reverse? (reverse)))))
        (cond->>
          marker (drop-while (fn [entry]
                               (if reverse?
                                 (< marker (time-key entry))
                                 (> marker (time-key entry)))))))))


(defn get-prices
  "Returns a lazy sequence of prices for the given commodity."
  [data commodity opts]
  ; TODO: bind graph?
  (year-seek
    (merkle/get-path (:graph data) @(:root data) "prices" commodity)
    opts))


(defn get-quote
  "Looks up the most recent price for the given commodity."
  ([data commodity]
   (lookup-price data commodity (time/now)))
  ([data commodity time]
   (let [prices (merkle/get-path (:graph data) @(:root data) "prices" commodity)]
     '...)))


(defn validate
  "Validates an entire financial dataset. Returns a lazy sequence of validation
  errors which are generated as the sequence is consumed."
  [root]
  ; FIXME: implement
  nil)


; Goal: integrate : Root , String , String , LedgerEntries -> NewRoot
; Function which takes:
; - current financial root multihash
; - string naming the books to use (e.g., "personal")
; - string naming the journal to put the transactions in (e.g., "general")
; - collection of ledger entries
; Returns the updated finances root multihash.
(defn integrate-entries
  [root books journal entries]
  ; FIXME: implement
  root)
