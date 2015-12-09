(ns merkledag.data.finance.quantity
  "Financial quantity type definition.")


(defrecord Quantity
  [value commodity])


(defmethod print-method Quantity
  [quantity writer]
  (print-method (tagged-literal 'finance/$ [(:value quantity) (:commodity quantity)])))
