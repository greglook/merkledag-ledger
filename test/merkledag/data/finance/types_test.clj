(ns merkledag.data.finance.types-test
  (:require
    [clojure.test :refer :all]
    [merkledag.data.finance.types :as types]))


(deftest quantity-serialization
  (testing "EDN representation"
    (let [q1 (types/->Quantity 1.0M 'USD)]
      (is (nil? (types/quantity->form nil))
          "represents nil as nil")
      (is (= [1.0M 'USD] (types/quantity->form q1)))
      (is (= q1 (types/form->quantity [1.0M 'USD])))))
  (testing "printed representation"
    (is (= "#finance/$ [3.14M JPY]" (pr-str (types/->Quantity 3.14M 'JPY))))
    (is (= "#finance/$ [0 USD]" (pr-str (types/->Quantity 0 'USD))))))
