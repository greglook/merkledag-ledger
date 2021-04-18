(ns finance.data.quantity-test
  (:require
    [clojure.test :refer [deftest testing is]]
    [finance.data.quantity :as quantity]))


(deftest encoding
  (testing "EDN representation"
    (let [q1 (quantity/q 1.0M 'USD)]
      (is (nil? (quantity/quantity->form nil))
          "represents nil as nil")
      (is (= [1.0M 'USD] (quantity/quantity->form q1)))
      (is (= q1 (quantity/form->quantity [1.0M 'USD]))))))


(deftest printing
  (testing "printed representation"
    (is (= "#finance/q [3.14M JPY]" (pr-str (quantity/q 3.14M 'JPY))))
    (is (= "#finance/q [0 USD]" (pr-str (quantity/q 0 'USD))))))
