(ns merkledag.data.finance
  "General utilities for working with the merkledag finance system."
  (:require
    [datascript.core :as d]
    [merkledag.data.finance.schema :as mdfs]
    [merkledag.data.finance.types :as types]
    [multihash.core :as multihash])
  (:import
    merkledag.data.finance.types.Quantity
    multihash.core.Multihash))


(def data-types
  {types/quantity-tag
   {:description "financial quantity"
    :reader types/form->quantity
    :writers {Quantity types/quantity->form}}})
