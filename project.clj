(defproject mvxcvi/merkledag-finance "0.1.0-SNAPSHOT"
  :description "Financial plugin for merkledag."
  :url "https://github.com/greglook/merkledag-finance"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]

  :dependencies
  [[clj-time "0.11.0"]
   [instaparse "1.4.1"]
   [mvxcvi/blocks "0.5.0"]
   [mvxcvi/merkledag-repo "0.1.0"] ; TODO: change to mvxcvi/merkledag
   [mvxcvi/puget "1.0.0"]
   [org.clojure/clojure "1.7.0"]
   [prismatic/schema "1.0.3"]]

  :whidbey
  {:tag-types {'org.joda.time.DateTime {'inst str}
               'org.joda.time.LocalDate {'time/date str}
               'org.joda.time.Interval {'time/interval #(vector (clj-time.core/start %) (clj-time.core/end %))}
               'blocks.data.Block {'blocks.data.Block (partial into {})}
               'merkledag.data.finance.quantity.Quantity {'finance/$ (juxt :value :commodity)}
               'merkledag.link.MerkleLink {'data/link (juxt :name :target :tsize)}
               'multihash.core.Multihash {'data/hash 'multihash.core/base58}}}

  :profiles
  {:repl {:source-paths ["dev"]}})
