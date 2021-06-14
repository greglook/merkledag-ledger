(ns finance.data.commodity-test
  (:require
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
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


(deftest distributions
  (let [spec (#'commodity/distribution-map-spec #{:a :b :c})
        m (rand-nth (gen/sample (s/gen spec)))]
    (is (map? m))
    (is (seq m))
    (is (== 1 (reduce + (vals m))))))
