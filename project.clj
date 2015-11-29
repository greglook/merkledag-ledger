(defproject mvxcvi/merkledag-finance "0.1.0-SNAPSHOT"
  :description "Financial plugin for merkledag."
  :url "https://github.com/greglook/merkledag-finance"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]

  :plugins
  [[lein-antlr "0.3.0"]]

  :dependencies
  [[mvxcvi/blocks "0.5.0"]
   [mvxcvi/merkledag-repo "0.1.0"] ; TODO: change to mvxcvi/merkledag
   [mvxcvi/puget "1.0.0"]
   [org.clojure/clojure "1.7.0"]]

  :antlr-src-dir "resources/antlr"
  :antlr-dest-dir "target/antlr-src"
  :antlr-options {:Werror true}

  :whidbey
  {:tag-types {'blocks.data.Block {'blocks.data.Block (partial into {})}
               'merkledag.link.MerkleLink {'data/link (juxt :name :target :tsize)}
               'multihash.core.Multihash {'data/hash 'multihash.core/base58}}}

  :profiles
  {:repl {:source-paths ["dev"]}})
