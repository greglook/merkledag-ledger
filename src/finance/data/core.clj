(ns finance.data.core
  "Core data definitions for the financial data schema and related helpers."
  (:require
    [alphabase.base32 :as b32]
    [alphabase.bytes :as bytes]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as str]))


;; ## Data Attributes

(def attributes
  "Map of all defined attributes and their datascript definitions."
  {})


(defmacro defattr
  "Define and register a new schema attribute, along with the related spec."
  [attr-key doc-str spec & {:as opts}]
  {:pre [(qualified-keyword? attr-key)
         (string? doc-str)]}
  `(do
     (s/def ~attr-key ~spec)
     (alter-var-root
       #'attributes assoc ~attr-key
       ~(assoc opts :db/doc doc-str))
     ~attr-key))


(defattr ::type
  "Keyword identifying the type of entity."
  qualified-keyword?
  :db/index true)


;; ## Entity Identifiers

(defn gen-id
  "Generates a random identifier string with the provided prefix and optional
  entropy in bytes."
  ([prefix]
   (gen-id prefix 8))
  ([prefix length]
   ;; TODO: time components?
   (str prefix "-" (str/lower-case (b32/encode (bytes/random-bytes length))))))


(defmacro defident
  "Define a new unique identifier attribute."
  [id-key doc-str prefix & {:keys [id-length], :or {id-length 8}}]
  {:pre [(qualified-keyword? id-key)
         (string? doc-str)
         (string? prefix)
         (pos-int? id-length)]}
  `(do
     (defn ~'gen-id
       ~(str "Generate a new " id-key " identifier.")
       []
       (gen-id ~prefix ~id-length))

     (defattr ~id-key
       ~doc-str
       (s/with-gen
         (s/and string? (fn [id#] (str/starts-with? id# ~(str prefix "-"))))
         #(gen/fmap
            (partial str ~prefix "-")
            (gen/string-alphanumeric)))
       :db/unique :db.unique/identity)))


;; ## Normal Form

;; The 'normal' form is an expanded representation of the dataset where every
;; entity is a separate map, linked by unique identifier attributes. For
;; example, entries would include a `::transaction/id` matching the one
;; assigned to the transaction they are part of.

(defmulti normal-form
  "Return a spec for the normal form of an entity type."
  ::type)


(defn ^:no-doc only-keys?
  "True if the provided `entity` only contains keys in the set `ks`."
  [ks entity]
  (->> (keys entity)
       (remove (set ks))
       (empty?)))


(defmacro defentity
  "Define a normal form spec for an entity type, ensuring that all attributes
  given are defined. The spec also ensures that the entity contains no
  undeclared attributes."
  [type-key & {:keys [req opt]}]
  ;; TODO: could support and/or combinators here
  (let [entity-attrs (set (concat req opt))]
    (when-let [undeclared (seq (remove attributes entity-attrs))]
      (throw (ex-info (str type-key " entity defined using undeclared attributes: "
                           (str/join " " (sort undeclared)))
                      {:undeclared undeclared})))
    `(let [~'only-declared-keys? (partial only-keys? ~entity-attrs)]
       (defmethod normal-form ~type-key
         [_#]
         (s/and (s/keys :req ~(vec req)
                        :opt ~(vec opt))
                ~'only-declared-keys?)))))


;; ## Tree Form

;; The 'tree' form is a structure for the dataset in which relationships are
;; expressed by direct embedding, forming a tree of entities. For example, the
;; entries for a transaction are represented as a sequence directly set on the
;; transaction map. This is closest in form to what is represented in a textual
;; format like ledger.

(defmulti tree-form
  "Return a spec for the tree form of an entity type."
  ::type)


(defn tree-spec
  "Construct a spec that asserts the values are tree entities with the given
  type."
  [type-key]
  (s/and (s/multi-spec tree-form ::type)
         ;#(isa? (::type %) type-key)
         #(= type-key (::type %))))


(defmulti normalize-tree
  "Expand the tree-form of an entity into a sequence of norm-form entity maps."
  (fn dispatch
    [_ctx data]
    (::type data)))


;; ## Tuple Form

(defmacro defref
  "Define and register a new schema reference, which links entities together.
  When in normal form, the `ref-id` keyword will be used to link the entities
  together; in the datascript db, the `ref-attr` will reference the
  corresponding entity directly."
  [ref-attr doc-str ref-id & {:as opts}]
  `(alter-var-root
     #'attributes assoc ~ref-attr
     ~(assoc opts
             ::ref-id ref-id
             :db/doc doc-str
             :db/valueType :db.type/ref)))


;; TODO: should the translation to EAV happen here? :thinking:
;; TODO: does this even need to be a multimethod?
#_
(defmulti entity-tuples
  "Return a sequence of EAV tuples about the given normal-form entity."
  (fn dispatch
    [db entity]
    (::type entity)))


(defn project-attributes
  "Take an entity map and project it into a collection of EAV tuples."
  [entity]
  (mapcat
    (fn expand
      [[k v]]
      ,,,)
    entity))


;; ## Utilities

(s/def ::some-string
  (s/and string? (complement str/blank?)))


#_
(defattr ::tags
  "Map of keyword tags to string values. Primarily used for indexing related
  entities."
  (s/map-of keyword? string?))
