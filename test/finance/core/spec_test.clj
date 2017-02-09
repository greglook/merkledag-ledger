(ns finance.core.spec-test
  (:require
    [clojure.test :refer :all]
    [finance.core.spec :as spec]))


(deftest validate-asset-class-tree
  (letfn [(visit-node
            [[class-name & children :as node]]
            (testing class-name
              (if (empty? children)
                (is (spec/asset-classes class-name)
                    (str "leaf nodes must be members of asset-classes"))
                (do
                  (is (not (spec/asset-classes class-name))
                      "group nodes must not be members of asset-classes")
                  (run! visit-node children)))))]
   (visit-node spec/asset-class-tree)))
