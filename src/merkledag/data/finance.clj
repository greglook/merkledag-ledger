(ns merkledag.data.finance
  "General utilities for working with the merkledag finance system."
  (:require
    [merkledag.data.finance.quantity])
  (:import
    merkledag.data.finance.quantity.Quantity))


(def data-types
  {'finance/$ {:description "financial quantity"
               :reader #(Quantity. (first %) (second %))
               :writers {Quantity (juxt :value :commodity)}}})
