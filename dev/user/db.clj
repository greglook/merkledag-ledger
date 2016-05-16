(ns user.db
  "Separate namespace to hold the current database, so that `tools.namespace`
  won't reload it."
  (:require
    [clojure.tools.namespace.repl :as ctnr]
    [datascript.core :as d]))


(ctnr/disable-reload!)


(defn- create-db
  "Creates a new empty database from the `db-schema` var."
  []
  (let [schema-var (resolve 'merkledag.data.finance.schema/db-schema)]
    (d/create-conn @schema-var)))


(def conn
  "REPL database atom."
  (create-db))


(defn reset-db!
  "Replaces the current database with a fresh one."
  []
  (alter-var-root #'conn (constantly (create-db)))
  nil)
