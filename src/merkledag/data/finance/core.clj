(ns merkledag.data.finance.core
  "..."
  (:require
    [blocks.core :as block]
    [clj-time.core :as time]
    [merkledag.core :as merkle]))


(defn ->finance-data
  [graph root]
  {:graph graph
   :root (atom root)})


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
