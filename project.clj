(defproject sundbry/autobahn.cljs "0.1.1-SNAPSHOT"
  :description "Clojurescript bindings for autobahn.js"
  :plugins [[lein-cljsbuild "1.0.4"]]
  :repositories [["snapshots" {:url "http://mises.etheride.com:8081/nexus/content/repositories/snapshots"}]]
  :dependencies []
  :source-paths ["src"]
  :cljsbuild {:builds
              [{:source-paths ["src"]
                :id :client
                :compiler
                {:output-to "resources/autobahn_cljs.js"
                 :output-dir "build"
                 :optimizations :whitespace
                 :preamble ["autobahn.min.js"]
                 }}]}
  :profiles
  {:dev
   {:dependencies [[org.clojure/clojure "1.6.0"]
                   [org.clojure/clojurescript "0.0-2760"]
                   [org.clojure/core.async "0.1.346.0-17112a-alpha"]]}})
