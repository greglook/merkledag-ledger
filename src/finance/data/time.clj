(ns finance.data.time
  "Shared time-related data specs."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen])
  (:import
    (java.time
      Instant
      LocalDate)))


;; ## Data Specs

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



;; ## Time Utilities

(defn now
  "Return the current instant in time."
  ^Instant
  []
  (Instant/now))


(defn today
  "Return the current local date."
  ^LocalDate
  []
  ,,,)



;; ## Coercions

(defn parse-instant
  "Parse a string as an instant in time."
  ^Instant
  [string]
  (Instant/parse string))


(defn parse-local-date
  "Parse a string as a local date."
  ^LocalDate
  [string]
  (LocalDate/parse string))
