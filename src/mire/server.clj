(ns mire.server
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [server.socket :as socket]))

(def prompt "> ")

(defn- handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]
    (println "Welcome to Mire! Type 'help' to see available commands.")
    (print prompt)
    (flush)
    (loop [line (read-line)]
      (when line
        (let [trimmed (str/trim line)]
          (cond
            (= trimmed "quit")
            (println "Goodbye!")

            (= trimmed "help")
            (do (println "Available commands: help, quit")
                (print prompt)
                (flush)
                (recur (read-line)))

            (str/blank? trimmed)
            (do (print prompt)
                (flush)
                (recur (read-line)))

            :else
            (do (println "Nothing happens.")
                (print prompt)
                (flush)
                (recur (read-line)))))))))

(defn -main
  ([] (-main 3333))
  ([port]
   (println "Starting Mire basic server on port" port)
   (socket/create-server (Integer. port) handle-client)))
