(ns finance.data.time
  "Shared time-related data specs."
  (:require
    [clojure.spec :as s])
  (:import
    (java.time
      Instant
      LocalDate)))


(s/def ::instant
  "Instant in time."
  inst?)


(s/def ::local-date
  "Calendar date associated with the entity."
  #(instance? LocalDate %))


#_
(defattr :time/interval
  "Interval in time the data occurred over."
  (s/with-gen
    #(instance? Interval %)
    #(gen/fmap
       (fn [times]
         (let [[a b] (sort times)]
           (Interval. a b)))
       (gen/vector (s/gen :time/at) 2))))
