(ns finance.repl.db
  "Separate namespace to hold the current database, so that `tools.namespace`
  won't reload it."
  (:require
    [clojure.tools.namespace.repl :as ns.repl]
    [datascript.core :as ds]))


(ns.repl/disable-reload!)


(defn- create-db
  "Creates a new empty database from the `db-schema` var."
  []
  (let [schema-var (resolve 'finance.core.spec/attributes)]
    (ds/create-conn @schema-var)))


(def conn
  "REPL database atom."
  (create-db))


(defn reset-db!
  "Replaces the current database with a fresh one."
  []
  (alter-var-root #'conn (constantly (create-db)))
  nil)
