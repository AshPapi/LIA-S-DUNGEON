(ns mire.server
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]))

(defn- cleanup []
  (when-let [room @player/*current-room*]
    (doseq [item @player/*inventory*]
      (commands/drop (name item)))
    (dosync
     (commute (:inhabitants room) disj player/*name*))))

(defn- game-loop []
  (println "\nWelcome to Mire. Type 'help' for a list of commands.")
  (println (commands/look))
  (print player/prompt)
  (flush)
  (loop [line (read-line)]
    (when line
      (let [trimmed (str/trim line)]
        (if (= trimmed "quit")
          (println "Goodbye!")
          (do
            (when-not (str/blank? trimmed)
              (println (commands/execute trimmed)))
            (print player/prompt)
            (flush)
            (recur (read-line))))))))

(defn- start-game []
  (let [start-room (rooms/current :start)]
    (dosync
     (ref-set player/*current-room* start-room)
     (ref-set player/*inventory* #{})
     (commute (:inhabitants @player/*current-room*) conj player/*name*))
    (game-loop)))

(defn- mire-handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]
    (print "What is your name? ")
    (flush)
    (let [name (or (some-> (read-line) str/trim) "Wanderer")]
      (binding [player/*name* (if (str/blank? name) "Wanderer" name)
                player/*current-room* (ref nil)
                player/*inventory* (ref #{})]
        (try
          (start-game)
          (finally (cleanup)))))))

(defn -main
  ([] (-main 3333))
  ([port]
   (rooms/add-rooms)
   (println "Starting Mire server with items and inventory on port" port)
   (socket/create-server (Integer. port) mire-handle-client)))
