(defproject mvxcvi/finance "0.2.0-SNAPSHOT"
  :description "Financial data modeling."
  :url "https://github.com/greglook/merkledag-finance"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]

  :aliases
  {"kaocha" ["with-profile" "+kaocha" "run" "-m" "kaocha.runner"]}

  :dependencies
  [[org.clojure/clojure "1.10.2"]
   [datascript "1.0.4"]
   [instaparse "1.4.10"]]

  :hiera
  {:vertical false
   :cluster-depth 4
   :ignore-ns #{clojure}
   :show-external false}

  :whidbey
  {:tag-types {'finance.types.Quantity {'finance/q (juxt :value :commodity)}}}

  :profiles
  {:repl
   {:source-paths ["dev"]
    :repl-options {:init-ns finance.repl}
    :jvm-opts
    ["-XX:-OmitStackTraceInFastThrow"]
    :dependencies
    [[org.clojure/tools.namespace "1.0.0"]
     [clj-stacktrace "0.2.8"]
     [mvxcvi/puget "1.3.1"]]}

   :kaocha
   {:dependencies
    [[lambdaisland/kaocha "1.0.641"]]}})
