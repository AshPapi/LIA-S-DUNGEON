(defproject mire "0.1.0"
  :description "Initial text server that accepts connections and handles simple commands."
  :main ^:skip-aot mire.server
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [server-socket "1.0.0"]])
