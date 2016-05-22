(ns merkledag.data.finance.ledger.parse-test
  (:require
    [clj-time.core :as time]
    [clojure.test :refer :all]
    [merkledag.data.finance.ledger.parse :as parse]
    [merkledag.data.finance.schema :as schema]
    [merkledag.data.finance.types :as types]
    [schema.core :as s]))


(defn- test-parse
  [source & values]
  (is (= values (parse/parse-group source)))
  (doseq [entry values]
    (when-let [schema (case (:data/type entry)
                        :finance/commodity   schema/CommodityDefinition
                        :finance/price       schema/CommodityPrice
                        :finance/account     schema/AccountDefinition
                        :finance/transaction schema/Transaction
                        nil)]
      (s/validate schema entry))))


(deftest comment-parsing
  (test-parse
    ";;;;; Foo Bar ;;;;;\n"
    [:CommentHeader "Foo Bar"])
  (test-parse
    "; This is a comment\n; spanning multiple lines.\n"
    [:CommentBlock "This is a comment" "spanning multiple lines."]))


(deftest commodity-declarations
  (test-parse
    "commodity FOO\n"
    {:data/type :finance/commodity
     :finance.commodity/code 'FOO})
  (test-parse
    "commodity $
    note United States Dollars
    note type: currency
    note class: cash
    format $1,000.00
    nomarket
    default\n"
    {:title "United States Dollars"
     :data/type :finance/commodity
     :finance.commodity/code 'USD
     :finance.commodity/currency-symbol "$"
     :finance.commodity/type :finance.commodity.type/currency
     :finance.commodity/class :finance.commodity.class/cash})
  (test-parse
    "commodity \"VTR2050\"
    note Vanguard Target Retirement 2050 Fund Tr II
    note type: mutual-fund\n"
    {:title "Vanguard Target Retirement 2050 Fund Tr II"
     :data/type :finance/commodity
     :finance.commodity/code 'VTR2050
     :finance.commodity/type :finance.commodity.type/mutual-fund})
  (let [source "commodity 1234\n"]
    (is (thrown? Exception (parse/parse-group source)))))


(deftest price-history
  (test-parse
    "P 2004-01-01 points      $0.01\n"
    {:data/type :finance/price
     :time/at (time/from-time-zone (time/date-time 2004 1 1)
                                   (time/default-time-zone))
     :finance.price/commodity 'points
     :finance.price/value (types/->Quantity 0.01M 'USD)})
  (test-parse
    "P 2016-05-20 17:05:30 TSLA      $220.28\n"
    {:data/type :finance/price
     :time/at (time/from-time-zone (time/date-time 2016 5 20 17 5 30)
                                   (time/default-time-zone))
     :finance.price/commodity 'TSLA
     :finance.price/value (types/->Quantity 220.28M 'USD)})
  (test-parse
    "P 2015-09-10 fooberries 101.01 XYZ\n"
    {:data/type :finance/price
     :time/at (time/from-time-zone (time/date-time 2015 9 10)
                                   (time/default-time-zone))
     :finance.price/commodity 'fooberries
     :finance.price/value (types/->Quantity 101.01M 'XYZ)}))


(deftest account-directives
  (test-parse
    "account Equity:Capital Gains\n"
    {:title "Capital Gains"
     :data/type :finance/account
     :finance.account/path ["Equity" "Capital Gains"]})
  (test-parse
    "account Assets:Cash:Big Apple Bank:Personal Checking
    alias apple-checking
    assert commodity == \"$\"
    note type: checking
    note external-id: XX01-13924280
    note link: d2df7edb50a138cc753e60ce4bb0beb9
    note Personal checking account.\n"
    {:title "Personal Checking"
     :description "Personal checking account."
     :data/type :finance/account
     :finance.account/path ["Assets" "Cash" "Big Apple Bank" "Personal Checking"]
     :finance.account/alias :apple-checking
     :finance.account/type :finance.account.type/checking
     :finance.account/commodities #{'USD}
     :finance.account/external-id "XX01-13924280"
     :finance.account/links #{"d2df7edb50a138cc753e60ce4bb0beb9"}}))


(deftest transaction-parsing
  '...)
