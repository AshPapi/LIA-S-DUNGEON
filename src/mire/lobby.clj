(ns mire.lobby
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [mire.player :as player]))

(def prompt "lobby> ")
(defonce state (ref {:players {}}))

(defn- broadcast [msg]
  (doseq [name (keys (:players @state))]
    (when-let [out (@player/streams name)]
      (binding [*out* out]
        (println msg)
        (print prompt)
        (flush)))))

(defn status []
  (let [players (:players @state)
        total (count players)
        ready (->> players (filter (comp :ready? val)) (map key) set)
        waiting (set/difference (set (keys players)) ready)]
    (str "Players: " total
         (when (seq ready)
           (str " | Ready: " (str/join ", " (sort ready))))
         (when (seq waiting)
           (str " | Waiting: " (str/join ", " (sort waiting)))))))

(defn register-player! [name]
  (let [start (promise)]
    (dosync
     (alter state assoc-in [:players name] {:ready? false :start start}))
    (broadcast (str name " entered the lobby."))
    start))

(defn leave! [name]
  (let [removed (dosync
                 (when (get-in @state [:players name])
                   (alter state update :players dissoc name)
                   true))]
    (when removed
      (broadcast (str name " left the lobby."))
      true)))

(defn- try-start! []
  (let [to-start (dosync
                  (let [players (:players @state)]
                    (when (and (seq players)
                               (every? (comp :ready? val) players))
                      (alter state assoc :players {})
                      players)))]
    (when to-start
      (doseq [[name _] to-start]
        (when-let [out (@player/streams name)]
          (binding [*out* out]
            (println "All players ready! Starting the game...")
            (flush))))
      (doseq [[_ {:keys [start]}] to-start]
        (deliver start :start))
      true)))

(defn mark-ready! [name ready?]
  (if (get-in @state [:players name])
    (do
      (dosync
       (alter state assoc-in [:players name :ready?] ready?))
      (broadcast (str name " is now " (if ready? "ready" "waiting")))
      (try-start!)
      (if ready?
        "You are marked ready."
        "You are marked not ready."))
    "You are not in the lobby."))
