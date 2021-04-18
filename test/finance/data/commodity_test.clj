(ns finance.data.commodity-test
  (:require
    [clojure.test :refer [deftest testing is]]
    [finance.data.commodity :as commodity]))


(deftest asset-class-structure
  (letfn [(visit-node
            [[class-name & children :as node]]
            (testing class-name
              (if (seq children)
                (do
                  (is (not (contains? commodity/asset-classes class-name))
                      "group nodes must not be members of asset-classes")
                  (run! visit-node children))
                (is (contains? commodity/asset-classes class-name)
                    "leaf nodes must be members of asset-classes"))))]
    (visit-node commodity/asset-class-tree)))
