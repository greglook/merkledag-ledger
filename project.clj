(defproject mvxcvi/merkledag-finance "0.1.0-SNAPSHOT"
  :description "Financial plugin for merkledag."
  :url "https://github.com/greglook/merkledag-finance"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]

  :dependencies
  [[org.clojure/clojure "1.9.0-alpha14"]
   [clj-time "0.13.0"]
   [datascript "0.15.5"]
   [instaparse "1.4.5"]
   [mvxcvi/blocks "0.8.0"]
   [mvxcvi/merkledag "0.2.0-SNAPSHOT"]
   [mvxcvi/multihash "2.0.1"]
   [mvxcvi/puget "1.0.1"]]

  :hiera
  {:vertical false
   :cluster-depth 4
   :ignore-ns #{clojure}
   :show-external false}

  :whidbey
  {:tag-types {'org.joda.time.DateTime {'inst str}
               'org.joda.time.LocalDate {'time/date str}
               'org.joda.time.Interval {'time/interval #(vector (clj-time.core/start %) (clj-time.core/end %))}
               'blocks.data.Block {'blocks.data.Block (partial into {})}
               'merkledag.data.finance.types.Quantity {'finance/$ (juxt :value :commodity)}
               'merkledag.link.MerkleLink {'data/link (juxt :name :target :tsize)}
               'multihash.core.Multihash {'data/hash 'multihash.core/base58}}}

  :profiles
  {:repl {:source-paths ["dev"]}})
