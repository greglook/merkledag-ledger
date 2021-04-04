(ns finance.data.time
  "Shared time-related data specs."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen])
  (:import
    (java.time
      Instant
      LocalDate)))


;; Instant in time.
(s/def ::instant
  (s/with-gen
    #(instance? Instant %)
    (s/gen inst?)))


;; Calendar date associated with the entity.
(s/def ::local-date
  #(instance? LocalDate %))


;; Interval in time the data occurred over.
(s/def ::interval
  any?
  #_
  (s/with-gen
    #(instance? Interval %)
    #(gen/fmap
       (fn [times]
         (let [[a b] (sort times)]
           (Interval. a b)))
       (gen/vector (s/gen :time/at) 2))))
