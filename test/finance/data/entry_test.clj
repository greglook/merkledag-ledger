(ns finance.data.entry-test
  (:require
    [clojure.test :refer :all]
    [finance.data.entry :as entry]
    [finance.data.quantity :as quantity]
    [finance.data.time :as time]))


#_
(deftest entry-weights
  (testing "non-weighted entries"
    (is (nil? (entry/weight {:data/type :finance.entry/balance-check
                             :finance.balance/amount (types/q 0.0M 'JPY)})))
    (is (nil? (entry/weight {:data/type :finance.entry/posting
                             :finance.posting/virtual true
                             :finance.posting/amount (types/q 3.4M 'USD)})))
    (is (nil? (entry/weight {:data/type :finance.entry/posting}))))
  (testing "explicit weight"
    (is (= (types/q 42M 'XYZ)
           (entry/weight {:data/type :finance.entry/posting
                          :finance.posting/amount (types/q 0.0M 'USD)
                          :finance.posting/weight (types/q 42M 'XYZ)}))))
  (testing "price conversion"
    (is (= (types/q 300.00M 'USD)
           (entry/weight {:data/type :finance.entry/posting
                          :finance.posting/amount (types/q 10.0M 'VNQ)
                          :finance.posting/price (types/q 30.00M 'USD)}))))
  (testing "cost conversion"
    (is (= (types/q 320.00M 'GBP)
           (entry/weight {:data/type :finance.entry/posting
                          :finance.posting/amount (types/q 32M 'SCHZ)
                          :finance.posting/cost {:amount (types/q 10.00M 'GBP)}}))))
  (testing "plain amount"
    (is (= (types/q 80.00M 'ABCD)
           (entry/weight {:data/type :finance.entry/posting
                          :finance.posting/amount (types/q 80.00M 'ABCD)})))))


#_
(deftest interpolate-missing-amounts
  (testing "without any postings"
    (is (= [] (entry/interpolate-amounts []))))
  (testing "with one missing amount"
    (is (thrown? Exception
          (entry/interpolate-amounts
            [{:data/type :finance.entry/posting}]))))
  (testing "with no missing amounts"
    (is (= [{:data/type :finance.entry/posting
             :finance.posting/amount (types/q 10.00M 'USD)}
            {:data/type :finance.entry/posting
             :finance.posting/amount (types/q -10.00M 'USD)}]
           (entry/interpolate-amounts
             [{:data/type :finance.entry/posting
               :finance.posting/amount (types/q 10.00M 'USD)}
              {:data/type :finance.entry/posting
               :finance.posting/amount (types/q -10.00M 'USD)}]))))
  (testing "with one missing amount"
    (is (= [{:data/type :finance.entry/posting
             :finance.posting/amount (types/q 25M 'XYZ)}
            {:data/type :finance.entry/posting
             :finance.posting/amount (types/q -25M 'XYZ)}
            {:data/type :finance.entry/note
             :description "lorem ipsum"}]
           (entry/interpolate-amounts
             [{:data/type :finance.entry/posting
               :finance.posting/amount (types/q 25M 'XYZ)}
              {:data/type :finance.entry/posting}
              {:data/type :finance.entry/note
               :description "lorem ipsum"}]))))
  (testing "with two missing amounts"
    (is (thrown? Exception
          (entry/interpolate-amounts
            [{:data/type :finance.entry/posting}
             {:data/type :finance.entry/posting}]))))
  (testing "with multiple commodities"
    (is (thrown? Exception
          (entry/interpolate-amounts
            [{:data/type :finance.entry/posting
              :finance.posting/amount (types/q 10M 'ABC)}
             {:data/type :finance.entry/posting
              :finance.posting/amount (types/q 8.41M 'USD)}
             {:data/type :finance.entry/posting}])))))
