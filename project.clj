(defproject chbrown/parsatron "0.0.9-SNAPSHOT"
  :description "Clojure parser combinators"
  :url "https://github.com/chbrown/parsatron"
  :license {:name "Eclipse Public License"
            :url "https://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]]
  :plugins [[lein-cljsbuild "1.1.7"]]
  :cljsbuild {:builds [{:id "production"
                        :source-paths ["src"]
                        :compiler {:output-dir "target"
                                   :output-to "target/main.js"
                                   :optimizations :simple
                                   :pretty-print false}}
                       {:id "test"
                        :source-paths ["src" "test"]
                        :compiler {:output-dir "target/test"
                                   :output-to "target/test/main.js"
                                   :main parsatron.runner
                                   :optimizations :whitespace}}]}
  :profiles {:dev {:doo {:paths {:rhino "lein run -m org.mozilla.javascript.tools.shell.Main"}}
                   :source-paths ["dev"]
                   :plugins [[lein-doo "0.1.8"]
                             [lein-cloverage "1.0.10"]]}})
