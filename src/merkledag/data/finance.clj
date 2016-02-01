(ns merkledag.data.finance
  "General utilities for working with the merkledag finance system.")


;; ## Quantity Type

(defrecord Quantity
  [value commodity])


(defmethod print-method Quantity
  [quantity writer]
  (print-method (tagged-literal 'finance/$ [(:value quantity) (:commodity quantity)]) writer))


(def data-types
  {'finance/$ {:description "financial quantity"
               :reader #(->Quantity (first %) (second %))
               :writers {Quantity (juxt :value :commodity)}}})
