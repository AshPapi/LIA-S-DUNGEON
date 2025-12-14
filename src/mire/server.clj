(ns mire.server
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]
            [mire.lobby :as lobby]))

(defn- cleanup []
  (when-let [room @player/*current-room*]
    (doseq [item @player/*inventory*]
      (commands/drop (name item)))
    (dosync
     (commute (:inhabitants room) disj player/*name*)))
  (lobby/leave! player/*name*)
  (dosync
   (alter player/streams dissoc player/*name*)))

(defn- game-loop []
  (println "\nThe dungeon awaits. Explore and chat with other players!")
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

(defn- lobby-loop [start-signal]
  (println "\nYou are in the lobby. Type 'ready' when everyone is set.")
  (println (lobby/status))
  (print lobby/prompt)
  (flush)
  (loop []
    (cond
      (realized? start-signal)
      (do
        (println "\nAll players ready! Starting the game...")
        :start)

      :else
      (when-let [line (read-line)]
        (let [cmd (-> line str/trim str/lower-case)
              result (case cmd
                       "ready" (do (println (lobby/mark-ready! player/*name* true)) nil)
                       "unready" (do (println (lobby/mark-ready! player/*name* false)) nil)
                       "status" (do (println (lobby/status)) nil)
                       "help" (do (println "Lobby commands: ready, unready, status, quit.") nil)
                       "quit" (do (println "Goodbye!") (flush) :quit)
                       (do (println "Unknown lobby command.") nil))]
          (if (= result :quit)
            :quit
            (do
              (print lobby/prompt)
              (flush)
              (recur))))))))

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
        (dosync
         (alter player/streams assoc player/*name* *out*))
        (let [start-signal (lobby/register-player! player/*name*)]
          (try
            (case (lobby-loop start-signal)
              :quit nil
              :start (start-game)
              (start-game))
            (finally (cleanup))))))))

(defn -main
  ([] (-main 3333))
  ([port]
   (rooms/add-rooms)
   (println "Starting Mire server with procedural dungeon on port" port)
   (socket/create-server (Integer. port) mire-handle-client)))
