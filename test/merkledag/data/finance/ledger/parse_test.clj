(ns merkledag.data.finance.ledger.parse-test
  (:require
    [clj-time.core :as time]
    [clojure.test :refer :all]
    [merkledag.data.finance.ledger.parse :as parse]
    [merkledag.data.finance.schema :as schema]
    [merkledag.data.finance.types :as types]
    [schema.core :as s]))


(defn- test-parse
  "Parses the given source text and asserts that the results match the
  interpreted values given. Each value is checked against the relevant schema
  as well."
  [text & values]
  (let [text (str text "\n")]
    (is (= values (parse/parse-group text))))
  (doseq [entry values]
    (when-let [schema (case (:data/type entry)
                        :finance/commodity   schema/CommodityDefinition
                        :finance/price       schema/CommodityPrice
                        :finance/account     schema/AccountDefinition
                        :finance/transaction schema/Transaction
                        nil)]
      (s/validate schema entry))))


(defn- local-dt
  "Takes the same args as `date-time`, but returns the specified time in the
  users default time zone."
  [& args]
  (time/from-time-zone (apply time/date-time args) (time/default-time-zone)))


(deftest comment-parsing
  (test-parse
    ";;;;; Foo Bar ;;;;;"
    [:CommentHeader "Foo Bar"])
  (test-parse
    "; This is a comment\n; spanning multiple lines."
    [:CommentBlock "This is a comment" "spanning multiple lines."]))


(deftest commodity-declarations
  (test-parse
    "commodity FOO"
    {:data/type :finance/commodity
     :finance.commodity/code 'FOO})
  (test-parse
    "commodity $
    note United States Dollars
    note type: currency
    note class: cash
    format $1,000.00
    nomarket
    default"
    {:title "United States Dollars"
     :data/type :finance/commodity
     :finance.commodity/code 'USD
     :finance.commodity/currency-symbol "$"
     :finance.commodity/type :finance.commodity.type/currency
     :finance.commodity/class :finance.commodity.class/cash})
  (test-parse
    "commodity \"VTR2050\"
    note Vanguard Target Retirement 2050 Fund Tr II
    note type: mutual-fund"
    {:title "Vanguard Target Retirement 2050 Fund Tr II"
     :data/type :finance/commodity
     :finance.commodity/code 'VTR2050
     :finance.commodity/type :finance.commodity.type/mutual-fund})
  (let [source "commodity 1234"]
    (is (thrown? Exception (parse/parse-group source)))))


(deftest price-history
  (test-parse
    "P 2004-01-01 points      $0.01"
    {:data/type :finance/price
     :time/at (local-dt 2004 1 1)
     :finance.price/commodity 'points
     :finance.price/value (types/q 0.01M 'USD)})
  (test-parse
    "P 2016-05-20 17:05:30 TSLA      $220.28"
    {:data/type :finance/price
     :time/at (local-dt 2016 5 20 17 5 30)
     :finance.price/commodity 'TSLA
     :finance.price/value (types/q 220.28M 'USD)})
  (test-parse
    "P 2015-09-10 fooberries 101.01 XYZ"
    {:data/type :finance/price
     :time/at (local-dt 2015 9 10)
     :finance.price/commodity 'fooberries
     :finance.price/value (types/q 101.01M 'XYZ)}))


(deftest account-directives
  (test-parse
    "account Equity:Capital Gains"
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
    note Personal checking account."
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
