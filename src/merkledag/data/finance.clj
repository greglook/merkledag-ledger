(ns merkledag.data.finance
  "Data types for the merkledag finance system."
  (:require
    [finance.core.types :as types])
  (:import
    finance.core.types.Quantity))


(def data-types
  {types/quantity-tag
   {:description "financial quantity"
    :reader types/form->quantity
    :writers {Quantity types/quantity->form}}})
