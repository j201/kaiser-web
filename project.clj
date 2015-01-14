(defproject kaiser-web "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2665"]
                 [reagent "0.5.0-alpha"]]

  :node-dependencies [[source-map-support "0.2.8"]]

  :plugins [[lein-cljsbuild "1.0.4"]
            [lein-npm "0.4.0"]]

  :source-paths ["src" "target/classes"]

  :clean-targets ["out/kaiser_web" "kaiser_web.js" "kaiser_web.min.js"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src"]
              :compiler {
                :output-to "kaiser_web.js"
                :preamble ["reagent/react.js"]
                :output-dir "out"
                :optimizations :none
                :cache-analysis true                
                :source-map true}}
             {:id "release"
              :source-paths ["src"]
              :compiler {
                :output-to "kaiser_web.min.js"
                :pretty-print false              
                :optimizations :advanced}}]})
