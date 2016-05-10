(ns merkledag.data.finance
  "General utilities for working with the merkledag finance system."
  (:require
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



;; ## Data Integration

(comment
  (def ^:dynamic *book-name*
    "String naming the current set of books being parsed."
    nil)


  (defn integrater-dispatch
    "Selects an integration dispatch value based on the argument type."
    [_ entry]
    (if (vector? entry)
      (first entry)
      (:data/type entry)))


  (defmulti integrate-entry
    "Integrates the given entry into a data system."
    #'integrater-dispatch)


  (defmethod integrate-entry :default
    [data entry]
    (println "Ignoring unsupported entry" (integrater-dispatch data entry))
    data)


  (defmethod integrate-entry :CommentHeader
    [data entry]
    ; Ignored
    data)


  (defmethod integrate-entry :CommentBlock
    [data entry]
    ; Ignored
    data)


  (defmethod integrate-entry :finance/commodity
    [data entry]
    (let [code (:finance.commodity/code entry)
          current (get-in data [:commodities code])
          new-data (merge current (dissoc entry ::format ::options))]
      (if (= current new-data)
        data
        (assoc-in data [:commodities code] new-data))))


  (defmethod integrate-entry :finance/price
    [data entry]
    (let [code (:finance.price/commodity entry)
          year-path [:prices code (str (time/year (:time/at entry)))]
          prices (or (get-in data year-path)
                     {:data/type :finance/price-history
                      :finance.price/commodity code
                      :finance.price/points []})
          new-price {:time (ctime/to-date-time (:time/at entry))
                     :value (:finance.price/value entry)}
          new-prices (update prices :finance.price/points
                             #(->> (conj % new-price) (set) (sort-by :time) (vec)))]
      (assoc-in data year-path new-prices)))


  (defn get-account
    "Looks up an account by path, starting from the given set of accounts which
    are children of the current node."
    [accounts path]
    (when-let [account (first (filter #(= (first path) (:title %)) accounts))]
      (if-let [remaining (seq (rest path))]
        (recur (:group/children account) remaining)
        account)))


  (defn add-account
    "Merges a new account definition into a set of accounts, replacing any
    account at the same path and adding to sets where necessary."
    [accounts path new-account]
    (if-let [next-node (get-account accounts [(first path)])]
      ; node exists, merge
      (-> accounts
          (disj next-node)
          (conj (if (empty? path)
                  (if (= :finance/account (:data/type next-node))
                    (merge next-node new-account)
                    (throw (ex-info "Tried to add an account at an existing intermediate node!"
                                    {:new-account new-account, :node next-node})))
                  (if (= :finance/account-group (:data/type next-node))
                    (update next-node :group/children add-account (rest path) new-account)
                    (throw (ex-info "Tried to add an account as a child of a non-group node!"
                                    {:new-account new-account, :node next-node}))))))
      ; node does not exist, create and recurse
      (conj
        (set accounts)
        (if (empty? path)
          new-account
          {:title (first path)
           :data/type :finance/account-group
           :group/children (add-account nil (rest path) new-account)}))))


  (defmethod integrate-entry :finance/account
    [data entry]
    (when-not *book-name*
      (throw (RuntimeException. "Must bind *book-name* to integrate accounts!")))
    (let [path (::path entry)
          current-data (get-account (get-in data [:books *book-name* :accounts]) path)
          new-data (merge current-data (dissoc entry ::path))]
      (if (= current-data new-data)
        data
        (update-in data [:books *book-name* :accounts]
                   add-account (butlast path) new-data))))


  (defmethod integrate-entry :finance/transaction
    [data entry]
    (when-not *book-name*
      (throw (RuntimeException. "Must bind *book-name* to integrate transactions!")))
    (let [date (::date entry)
          entry (dissoc entry ::date)]
      (update-in data
        [:books *book-name* :ledger
         (str (time/year date))
         (format "%02d" (time/month date))
         (format "%02d" (time/day date))]
        (fnil update {:data/type :finance/ledger
                      :time/date date
                      :finance.ledger/transactions []})
        :finance.ledger/transactions
        #(vec (sort-by :time/at (conj % entry))))))
  )
