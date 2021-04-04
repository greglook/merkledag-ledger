(ns finance.data.balance
  "Balance checks provide a mechanism for asserting the state of an account at
  specific points in time. This is a tool for tying the data to known-good
  numbers verified in the real world."
  (:require
    [finance.data.core :refer [defattr]]
    [finance.data.entry :refer [defentry]]
    [finance.data.quantity :as quantity]))


(defattr ::amount
  "Amount of a certain commodity the account should contain."
  ::quantity/q)


(defentry :balance-check
  "Assert that the target account has exactly a certain amount of a commodity."
  :req [::amount])
