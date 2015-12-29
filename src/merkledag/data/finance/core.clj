(ns merkledag.data.finance.core
  "..."
  (:require
    [blocks.core :as block]
    [clj-time.core :as time]
    [merkledag.core :as merkle]
    [merkledag.data.finance.quantity]
    [merkledag.link :as link])
  (:import
    merkledag.data.finance.quantity.Quantity))


(defn ->finance-data
  [graph root]
  {:graph graph
   :root (atom root)})


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
  #_
  (year-seek
    (merkle/get-path (:graph data) @(:root data) "prices" commodity)
    opts))


(defn get-quote
  "Looks up the most recent price for the given commodity as of time `at`."
  ([data commodity]
   (get-quote data commodity (time/now)))
  ([data commodity at]
   (first (get-prices data commodity {:marker at, :time-key :time/at, :reverse true}))))


(defn add-price!
  "Adds a price to the given financial data. Updates the root pointer."
  [data commodity entry]
  (let [entry (-> (if (instance? Quantity entry)
                    {:finance.price/value entry}
                    entry)
                  (assoc entry
                    :data/type :finance/price
                    :finance.price/commodity commodity)
                  (as-> entry'
                    (cond-> entry'
                      (nil? (:time/at entry'))
                        (assoc :time/at (time/now)))))]
    ; TODO: assert price matches schema
    ; new price should go into /prices/XXX/YYYY positioned in correct vector point
    ; 1. look up /prices/ - if no link for commodity, goto 4
    ; 2. look up ./<commodity> - if no link for entry year, goto 4
    ; 3. look up ./<year> and get current year data
    ; 4. use current data (or []) and insert the entry at the correct point
    ; 5. create a new year node with updated data (keep links intact, if any)
    ; 6. create a new commodity-prices node with an updated year link
    ; 7. create a new price-data node with updated commodity link
    ; 8. create a new root node with updated prices link
    (comment
      (graph/update-in
        graph :finances
        ["prices" commodity (time/year (:time/at entry))]
        (fn [price-node]
          (if price-node
            (merkle/node*
              (:format graph)
              (:links price-node)
              (vec (sort-by :time/at (conj (:data price-node) entry))))
            (merkle/node*
              (:format graph)
              nil
              [entry])))))))


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
