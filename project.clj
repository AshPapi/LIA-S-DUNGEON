(defproject mire "0.5.0"
  :description "Switch to procedural dungeon generation and shared world state."
  :main ^:skip-aot mire.server
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [server-socket "1.0.0"]])
