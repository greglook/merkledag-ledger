(ns merkledag.data.finance
  "General utilities for working with the merkledag finance system."
  (:require
    [datascript.core :as d]
    [merkledag.data.finance.schema :as schema]
    [merkledag.data.finance.types :as types])
  (:import
    merkledag.data.finance.types.Quantity))


(def data-types
  {types/quantity-tag
   {:description "financial quantity"
    :reader types/form->quantity
    :writers {Quantity types/quantity->form}}})




;; ## Financial Database

(defn finance-db
  [{:keys [repo ref-name]}]
  {:repo repo
   :root ref-name
   :data (d/create-conn schema/db-schema)})



;; ## Query Functions

(defn select-transactions
  [db book & query-args]
  (let [query (if (and (= 1 (count query-args))
                       (map? query-args))
                query-args
                (apply hash-map query-args))]
    (->>
      ; TODO: figure out what query args are suitable
      ; minimally, filtering on date should work
      (d/q '{:find [?tx ?date (min ?time)]
             :in [$ ?book]
             :where [[?a :finance.account/book ?book]
                     [?e :finance.entry/account ?a]
                     [?e :time/at ?time]
                     [?tx :finance.transaction/entries ?e]
                     [?tx :finance.transaction/date ?date]]}
           db book)
      (sort-by (comp vec rest))
      (map (comp (partial d/entity db) first)))))
