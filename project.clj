(defproject chbrown/parsatron "0.0.9-SNAPSHOT"
  :description "Clojure parser combinators"
  :url "https://github.com/chbrown/parsatron"
  :license {:name "Eclipse Public License"
            :url "https://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2227"]]
  :plugins [[lein-cljsbuild "1.0.3"]]
  :source-paths ["src/clj" "src/cljs"]
  :test-paths ["test/clj"]
  :cljsbuild {:builds [{:source-paths ["src/cljs" "test/cljs"]
                        :compiler {:output-to "test/resources/parsatron_test.js"}}]})
