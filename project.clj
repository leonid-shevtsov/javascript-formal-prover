(defproject jsfp "1.0.0"
            :description "Javascript formal prover"
            :url "https://github.com/leonid-shevtsov/javascript-formal-prover"
            :license {:name "MIT License"
                      :url  "http://mit-license.org"}
            :dependencies [[org.clojure/clojure "1.6.0"]
                           [org.antlr/antlr4 "4.5"]
                           [log4j/log4j "1.2.17"]
                           [org.clojure/tools.logging "0.3.1"]]
            :main ^:skip-aot jsfp.core
            :target-path "target/%s"
            :profiles {:uberjar {:aot :all}
                       :dev     {:dependencies [[midje "1.6.3"]]}})
