(ns finance.data.core
  "Core data definitions for the financial data schema and related helpers."
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as str]))


;; ## Entity Attribute Macros

(def attributes
  "Map of all defined attributes and their datascript definitions."
  {})


(defmacro defattr
  "Define and register a new schema attribute, along with the related spec."
  [attr-key doc-str spec & {:as opts}]
  `(do
     (s/def ~attr-key ~spec)
     (alter-var-root #'attributes assoc ~attr-key ~(assoc opts :db/doc doc-str))))


(defmacro defentity
  "Define an entity with a collection of required and optional attributes."
  [type-key doc-str & key-args]
  `(s/def ~type-key
     (let [spec# (s/keys ~@key-args)]
       (s/with-gen
         (s/and spec# #(= ~type-key (::type %)))
         #(gen/fmap
            (fn [e#] (assoc e# ::type ~type-key))
            (s/gen spec#))))))



;; ## General Data Attributes

(s/def ::some-string
  (s/and string? (complement str/blank?)))


(defattr ::type
  "Keyword identifying the primary entity type."
  qualified-keyword?
  :db/index true)


(defattr ::ident
  "Unique identifier for data entities."
  ::some-string
  :db/unique :db.unique/value)


(defattr ::tags
  "Map of keyword tags to string values. Primarily used for indexing related
  entities."
  (s/map-of keyword? string?))
