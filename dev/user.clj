(ns user
  (:require
    [blocks.core :as block]
    [clojure.java.io :as io]
    [clojure.repl :refer :all]
    [clojure.stacktrace :refer [print-cause-trace]]
    [clojure.string :as str]
    [clojure.tools.namespace.repl :refer [refresh]]
    [instaparse.core :as insta]
    (merkledag
      [core :as merkle]
      [graph :as graph])
    [merkledag.data.finance.core :as finance]))


(defn reload-grammar!
  []
  (alter-var-root #'finance/ledger-parser (constantly (insta/parser (io/resource "grammar/ledger.bnf"))))
  :reloaded)


; TODO: define block store/graph repo
