(defproject relay-node "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [ubergraph "0.5.2"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [org.jordanlewis/data.union-find "0.1.0"]]
  :main ^:skip-aot relay-node.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
