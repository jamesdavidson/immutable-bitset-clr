(defproject immutable-bitset "0.1.5"
  :description "a bitset which is immutable"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[primitive-math "0.1.3"]]
  :global-vars {*warn-on-reflection* true}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.5.1"]
                                  [criterium "0.4.1"]
                                  [reiddraper/simple-check "0.5.1"]
                                  [collection-check "0.1.1-SNAPSHOT"]]}}
  :test-selectors {:default (complement :benchmark)
                   :benchmark :benchmark}
  :jvm-opts ^:replace ["-server"])
