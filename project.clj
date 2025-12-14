(defproject mire "0.4.0"
  :description "Adds a multiplayer lobby before entering the dungeon."
  :main ^:skip-aot mire.server
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [server-socket "1.0.0"]])
