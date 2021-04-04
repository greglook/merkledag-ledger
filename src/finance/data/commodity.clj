(ns finance.data.commodity
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as str]
    [finance.data.asset :as asset]
    [finance.data.core :as data :refer [defattr defentity]]))


(defattr ::name
  "Human-meaningful name of the commodity."
  ::data/some-string)


(defattr ::description
  "Long-form descriptive text for the commodity."
  string?)


(defattr ::code
  "Code symbol used to identify the commodity."
  (s/with-gen
    (s/and symbol? #(re-matches #"[a-zA-Z][a-zA-Z0-9_]*" (str %)))
    (let [number-chars (set (map char (range 48 58)))
          upper-chars (set (map char (range 65 91)))
          lower-chars (set (map char (range 97 123)))
          prefix-chars (set/union upper-chars lower-chars)
          body-chars (set/union number-chars upper-chars lower-chars #{\_})]
      #(gen/fmap
         (fn [[prefix body]]
           (symbol (apply str prefix body)))
         (gen/tuple (gen/elements prefix-chars)
                    (gen/vector (gen/elements body-chars))))))
  :db/unique :db.unique/identity)


(defattr ::symbol
  "One-character string to prefix currency amounts with."
  (s/with-gen
    (s/and string? #(= 1 (count %)))
    #(gen/fmap str (gen/char-ascii))))


;; TODO: clarify the relation between precision and tolerance
;; TODO: rename 'scale'?
;; TODO: this should also not prevent higher-precision values from appearing e.g. as commodity prices
(defattr ::precision
  "Number of decimal places to represent the commodity to."
  integer?)


(defentity :finance.data/commodity
  "..."
  :req [::name
        ::code
        ::asset/type]
  :opt [::description
        ::symbol
        ::precision
        ::asset/class
        ::asset/sector])
