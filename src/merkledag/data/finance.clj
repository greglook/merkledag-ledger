(ns merkledag.data.finance
  "General utilities for working with the merkledag finance system."
  (:require
    [datascript.core :as d]
    [merkledag.data.finance.schema :as schema]
    [merkledag.data.finance.types :as types])
  (:import
    merkledag.data.finance.types.Quantity))


(def data-types
  {types/quantity-tag
   {:description "financial quantity"
    :reader types/form->quantity
    :writers {Quantity types/quantity->form}}})



;; ## Financial Database

(defn finance-db
  [{:keys [repo ref-name]}]
  {:repo repo
   :root ref-name
   :data (d/create-conn schema/db-schema)})
