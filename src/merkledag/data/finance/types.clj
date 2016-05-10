(ns merkledag.data.finance.types
  "Data types used in the finance app.")


(def quantity-tag
  "Symbol used to identify quantity values."
  'finance/$)


(defrecord Quantity
  [value commodity])


(defn quantity->form
  "Renders a quantity value as a vector form."
  [q]
  (when q
    [(:value q) (:commodity q)]))


(defn form->quantity
  "Reads a vector form into a quantity type."
  [v]
  {:pre [(vector? v) (= 2 (count v))]}
  (Quantity. (first v) (second v)))


(defmethod print-method Quantity
  [quantity writer]
  (print-method (tagged-literal quantity-tag (quantity->form quantity)) writer))
