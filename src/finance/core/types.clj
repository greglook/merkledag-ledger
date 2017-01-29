(ns finance.core.types
  "Data types used in the finance app.")


(def quantity-tag
  "Symbol used to identify quantity values."
  'finance/$)


(defrecord Quantity
  [value commodity])


(defn quantity?
  "Predicate which returns true if the value `x` is a quantity record."
  [x]
  (instance? Quantity x))


(defn q
  "Constructs a new quantity from the given amount and commodity symbol."
  [amount commodity]
  {:pre [(number? amount) (symbol? commodity)]}
  (->Quantity amount commodity))


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
