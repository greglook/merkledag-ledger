(ns merkledag.data.finance.quantity
  "Financial quantity type definition.")


; TODO: move this to main finance ns?
(defrecord Quantity
  [value commodity])


(defmethod print-method Quantity
  [quantity writer]
  (print-method (tagged-literal 'finance/$ [(:value quantity) (:commodity quantity)]) writer))
