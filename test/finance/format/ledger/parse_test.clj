(ns finance.format.ledger.parse-test
  (:require
    [finance.data.time :as time]
    [clojure.spec.alpha :as s]
    [clojure.test :refer [deftest testing is]]
    [finance.data.quantity :as quantity]
    [finance.format.ledger.parse :as parse]))


#_
(defn- test-parse
  "Parses the given source text and asserts that the results match the
  interpreted values given. Each value is checked against the relevant spec as
  well."
  [text & values]
  (let [text (str text "\n")]
    (is (= values (parse/parse-group text))))
  (doseq [entry values]
    (when-let [spec (s/get-spec (:data/type entry))]
      (when-not (s/valid? spec entry)
        (throw (ex-info (s/explain-str spec entry)
                        {:spec spec, :entry entry}))))))


#_
(defn- local-dt
  "Takes the same args as `date-time`, but returns the specified time in the
  users default time zone."
  [& args]
  (time/from-time-zone (apply time/date-time args) (time/default-time-zone)))


#_
(deftest comment-parsing
  (test-parse
    ";;;;; Foo Bar ;;;;;"
    [:CommentHeader "Foo Bar"])
  (test-parse
    "; This is a comment\n; spanning multiple lines."
    [:CommentBlock "This is a comment" "spanning multiple lines."]))


#_
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


#_
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


#_
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


#_
(deftest transaction-parsing
  (test-parse
    "2009-08-01 * Opening Balance
    wallet                                 $20.00
    Equity:Opening Balances"
    {:title "Opening Balance"
     :data/type :finance/transaction
     :finance.transaction/date (time/local-date 2009 8 1)
     :finance.transaction/flag :cleared
     :finance.transaction/entries [{:data/type :finance.entry/posting
                                    :finance.entry/account :wallet
                                    :finance.posting/amount (types/q 20.00M 'USD)
                                    :time/at (local-dt 2009 8 1)}
                                   {:data/type :finance.entry/posting
                                    :finance.entry/account ["Equity" "Opening Balances"]
                                    :time/at (local-dt 2009 8 1)}]})
  (test-parse
    "2010-08-05 * Opened Account
    ; :estimate:
    [ally-savings]                    $0.00
        ; type: open-account"
    {:title "Opened Account"
     :data/type :finance/transaction
     :data/tags {:estimate true}
     :finance.transaction/date (time/local-date 2010 8 5)
     :finance.transaction/flag :cleared
     :finance.transaction/entries [{:data/type :finance.entry/open-account
                                    :finance.entry/account :ally-savings
                                    :time/at (local-dt 2010 8 5)}]})
  (test-parse
    "2013-12-07 Balance Assertions
    [apple-checking]                          0 = $120.00"
    {:title "Balance Assertions"
     :data/type :finance/transaction
     :finance.transaction/date (time/local-date 2013 12 7)
     :finance.transaction/entries [{:data/type :finance.entry/balance-check
                                    :finance.entry/account :apple-checking
                                    :finance.balance/amount (types/q 120.00M 'USD)
                                    :time/at (local-dt 2013 12 7)}]})
  (test-parse
    "2016-04-16 ! Uber
    ; time: 14:03
    Expenses:Transit:Taxi                  $8.19
    credit-card"
    {:title "Uber"
     :data/type :finance/transaction
     :finance.transaction/date (time/local-date 2016 4 16)
     :finance.transaction/flag :pending
     :finance.transaction/entries [{:data/type :finance.entry/posting
                                    :finance.entry/account ["Expenses" "Transit" "Taxi"]
                                    :finance.posting/amount (types/q 8.19M 'USD)
                                    :time/at (local-dt 2016 4 16 14 3)}
                                   {:data/type :finance.entry/posting
                                    :finance.entry/account :credit-card
                                    :time/at (local-dt 2016 4 16 14 3)}]})
  (test-parse
    "2014-01-04 * Food Kiosk
    ; time: 06:50:21 US/Eastern
    Expenses:Food:Fast Food             $7.03
    credit-card"
    {:title "Food Kiosk"
     :data/type :finance/transaction
     :finance.transaction/date (time/local-date 2014 1 4)
     :finance.transaction/flag :cleared
     :finance.transaction/entries [{:data/type :finance.entry/posting
                                    :finance.entry/account ["Expenses" "Food" "Fast Food"]
                                    :finance.posting/amount (types/q 7.03M 'USD)
                                    :time/at (time/from-time-zone
                                               (time/date-time 2014 1 4 6 50 21)
                                               (time/time-zone-for-id "US/Eastern"))}
                                   {:data/type :finance.entry/posting
                                    :finance.entry/account :credit-card
                                    :time/at (time/from-time-zone
                                               (time/date-time 2014 1 4 6 50 21)
                                               (time/time-zone-for-id "US/Eastern"))}]})
  (test-parse
    "2016-04-12 ! Cloudlift Cellars
    ; UUID: 62a367f0f26938b6aae1b961d1c130b9
    ; Spring wine club.
    Expenses:Entertainment:Alcohol                    $68.35
        ; item: 2014 Chardonnay                       $17.00 *
        ; item: 2013 Halcyon                          $25.50
        ; item: 2012 Panorama                         $22.10 *
        ; item: Seattle Sales Tax                      $3.75 ($39.10 @ 9.6%) <*>
    amex-blue-cash
        ; source: mint|foo-bar-baz"
    {:title "Cloudlift Cellars"
     :description "Spring wine club."
     :data/ident "finance:transaction:62a367f0f26938b6aae1b961d1c130b9"
     :data/type :finance/transaction
     :finance.transaction/date (time/local-date 2016 4 12)
     :finance.transaction/flag :pending
     :finance.transaction/entries
     [{:data/type :finance.entry/posting
       :time/at (local-dt 2016 4 12)
       :finance.entry/account ["Expenses" "Entertainment" "Alcohol"]
       :finance.posting/amount (types/q 68.35M 'USD)
       :finance.posting/invoice
       {:data/type :finance/invoice
        :finance.invoice/items
        [{:title "2014 Chardonnay"
          :data/type :finance/item
          :finance.item/tax-groups #{:*}
          :finance.item/total (types/q 17.00M 'USD)}
         {:title "2013 Halcyon"
          :data/type :finance/item
          :finance.item/total (types/q 25.50M 'USD)}
         {:title "2012 Panorama"
          :data/type :finance/item
          :finance.item/tax-groups #{:*}
          :finance.item/total (types/q 22.10M 'USD)}
         {:title "Seattle Sales Tax"
          :data/type :finance/item
          :finance.item/tax-applied :*
          :finance.item/amount (types/q 39.10M 'USD)
          :finance.item/price 0.096M
          :finance.item/total (types/q 3.75M 'USD)}]}}
      {:data/type :finance.entry/posting
       :time/at (local-dt 2016 4 12)
       :finance.entry/account :amex-blue-cash
       :finance.entry/source-lines #{[:mint "foo-bar-baz"]}}]})
  (test-parse
    "2016-02-11 * Roth IRA Contribution
    ; link: dedbeec489b8d7e36869666b8c445161
    (roth-contributions)                             $500.00
    vanguard-roth-ira                                $500.00
    apple-checking"
    {:title "Roth IRA Contribution"
     :data/type :finance/transaction
     :finance.transaction/date (time/local-date 2016 2 11)
     :finance.transaction/flag :cleared
     :finance.transaction/links #{"dedbeec489b8d7e36869666b8c445161"}
     :finance.transaction/entries
     [{:data/type :finance.entry/posting
       :finance.entry/account :roth-contributions
       :finance.posting/virtual true
       :finance.posting/amount (types/q 500.00M 'USD)
       :time/at (local-dt 2016 2 11)}
      {:data/type :finance.entry/posting
       :finance.entry/account :vanguard-roth-ira
       :finance.posting/amount (types/q 500.00M 'USD)
       :time/at (local-dt 2016 2 11)}
      {:data/type :finance.entry/posting
       :finance.entry/account :apple-checking
       :time/at (local-dt 2016 2 11)}]})
  (test-parse
    "2015-03-29 * Lending Club
    lending-club                               -15.81 LCNOTE {$1.00}
        ; Principal Received
    Income:Returns:Interest                           $-6.57
    Expenses:Fees:Service Charges                      $0.22
    lending-club                                      $22.16"
    {:title "Lending Club"
     :data/type :finance/transaction
     :finance.transaction/date (time/local-date 2015 3 29)
     :finance.transaction/flag :cleared
     :finance.transaction/entries
     [{:data/type :finance.entry/posting
       :description "Principal Received"
       :finance.entry/account :lending-club
       :finance.posting/amount (types/q -15.81M 'LCNOTE)
       :finance.posting/cost {:amount (types/q 1.00M 'USD)}
       :time/at (local-dt 2015 3 29)}
      {:data/type :finance.entry/posting
       :finance.entry/account ["Income" "Returns" "Interest"]
       :finance.posting/amount (types/q -6.57M 'USD)
       :time/at (local-dt 2015 3 29)}
      {:data/type :finance.entry/posting
       :finance.entry/account ["Expenses" "Fees" "Service Charges"]
       :finance.posting/amount (types/q 0.22M 'USD)
       :time/at (local-dt 2015 3 29)}
      {:data/type :finance.entry/posting
       :finance.entry/account :lending-club
       :finance.posting/amount (types/q 22.16M 'USD)
       :time/at (local-dt 2015 3 29)}]})
  (test-parse
    "2016-04-22 * SCHH - Sell
    traditional-ira                                $1,606.01
    Expenses:Fees:Service Charges                      $0.04
    Income:Returns:Capital Gains:Short Term          $-10.05
    traditional-ira                                 -40 SCHH {$39.90} [2016-01-05] @ $40.1513  ;  $1,606.05 (short-term gain $10.05)"
    {:title "SCHH - Sell"
     :data/type :finance/transaction
     :finance.transaction/date (time/local-date 2016 4 22)
     :finance.transaction/flag :cleared
     :finance.transaction/entries
     [{:data/type :finance.entry/posting
       :finance.entry/account :traditional-ira
       :finance.posting/amount (types/q 1606.01M 'USD)
       :time/at (local-dt 2016 4 22)}
      {:data/type :finance.entry/posting
       :finance.entry/account ["Expenses" "Fees" "Service Charges"]
       :finance.posting/amount (types/q 0.04M 'USD)
       :time/at (local-dt 2016 4 22)}
      {:data/type :finance.entry/posting
       :finance.entry/account ["Income" "Returns" "Capital Gains" "Short Term"]
       :finance.posting/amount (types/q -10.05M 'USD)
       :time/at (local-dt 2016 4 22)}
      {:data/type :finance.entry/posting
       :description " $1,606.05 (short-term gain $10.05)"
       :finance.entry/account :traditional-ira
       :finance.posting/amount (types/q -40M 'SCHH)
       :finance.posting/cost {:amount (types/q 39.90M 'USD)
                              :date (time/local-date 2016 1 5)}
       :finance.posting/price (types/q 40.1513M 'USD)
       :time/at (local-dt 2016 4 22)}]}))
