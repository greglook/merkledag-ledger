(ns finance.data.quantity
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [finance.data.commodity :as commodity]
    [finance.data.core :as data :refer [defattr defentity]]))


;; ## Quantity Type

(defrecord Quantity
  [value commodity])


(alter-meta! #'->Quantity assoc :private true)
(alter-meta! #'map->Quantity assoc :private true)


(defn q
  "Constructs a new quantity from the given amount and commodity symbol."
  [amount commodity]
  {:pre [(number? amount) (symbol? commodity)]}
  (->Quantity amount commodity))


(defn quantity?
  "Predicate which returns true if the value `x` is a quantity record."
  [x]
  (and (instance? Quantity x)
       (number? (:value x))
       (symbol? (:commodity x))))



;; ## Encoding

(def ^:const quantity-tag
  "Symbol used to identify quantity values."
  'finance/q)


(defn quantity->form
  "Renders a quantity value as a vector form."
  [q]
  (when q
    [(:value q) (:commodity q)]))


(defn form->quantity
  "Reads a vector form into a quantity type."
  [v]
  {:pre [(vector? v) (= 2 (count v))]}
  (->Quantity (first v) (second v)))


;; TODO: human friendly printed rep?
(defmethod print-method Quantity
  [quantity writer]
  (print-method (tagged-literal quantity-tag (quantity->form quantity)) writer))



;; ## Data Specs

(s/def ::value number?)


(s/def ::commodity ::commodity/code)


(s/def ::q
  (s/with-gen
    (s/and quantity?
           (s/keys :req-un [::value
                            ::commodity]))
    #(gen/fmap
       (fn [[base exp code]]
         (q (nth (iterate (fn [x] (/ x 10)) (bigdec base))
                 exp)
            code))
       (gen/tuple (gen/large-integer)
                  (gen/large-integer* {:min 0, :max 3})
                  (s/gen ::commodity/code)))))
