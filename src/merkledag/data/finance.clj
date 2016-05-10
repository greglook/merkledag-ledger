(ns merkledag.data.finance
  "General utilities for working with the merkledag finance system."
  (:require
    [clojure.string :as str]
    [datascript.core :as d]
    [merkledag.data.finance.schema :as mdfs]
    [merkledag.data.finance.types :as types]
    [multihash.core :as multihash])
  (:import
    merkledag.data.finance.types.Quantity
    multihash.core.Multihash))


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
   :data (d/create-conn (merge mdfs/general-attrs
                               mdfs/commodity-attrs
                               mdfs/price-attrs))})



;; ## Account Functions

(defn get-account
  "Retrieves account data by either a multihash identifying the root block,
  a keyword alias, or a path vector of name segments."
  [db id]
  (cond
    (instance? Multihash id)
      (let [accounts (-> db :db deref :accounts)]
        (first (filter #(= id (:finance.account/id %)) (vals accounts))))

    (keyword? id)
      (let [accounts (-> db :db deref :accounts)]
        (first (filter #(= id (:finance.account/alias %)) (vals accounts))))

    (vector? id)
      (get @(:db db) id)

    :else
      (throw (ex-info (str "Illegal account identifier: " (pr-str id))
                      {:id id}))))
