(ns mire.lobby
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [mire.player :as player]))

(def prompt "lobby> ")

(defonce state (ref {:players {} :starting? false :countdown-start nil}))

(defn- all-ready? [s]
  (let [players (:players s)]
    (and (seq players) (every? (comp :ready? val) players))))

(defn- ready-names [s]
  (->> (:players s) (filter (comp :ready? val)) (map key)))

(defn- broadcast-to-lobby [msg]
  (doseq [name (keys (:players @state))]
    (when-let [out (@player/streams name)]
      (binding [*out* out]
        (println msg)
        (print prompt)
        (flush)))))

(defn status
  "Return a lobby status string with player counts, readiness, and countdown."
  []
  (let [s @state
        total (count (:players s))
        ready (set (ready-names s))
        waiting (sort (set/difference (set (keys (:players s))) ready))
        countdown (when-let [started (:countdown-start s)]
                    (let [elapsed (/ (- (System/currentTimeMillis) started) 1000.0)
                          left (Math/ceil (max 0 (- 5 elapsed)))]
                      (int left)))]
    (str "Players in lobby: " total " (ready: " (count ready) "). "
         (when (seq ready)
           (str "Ready: " (str/join ", " (sort ready)) ". "))
         (when (seq waiting)
           (str "Waiting: " (str/join ", " waiting) ". "))
         (when countdown
           (str "Starting in ~" countdown "s if everyone stays ready.")))))

(defn register-player!
  "Add a player to the lobby and return a promise that will be delivered when the game starts."
  [name]
  (let [start (promise)]
    (dosync
     (alter state
            (fn [s]
              (-> s
                  (assoc-in [:players name] {:ready? false :start start})
                  (assoc :starting? false :countdown-start nil)))))
    (broadcast-to-lobby (str name " joined the lobby."))
    start))

(defn leave!
  "Remove a player from the lobby and cancel any pending countdown."
  [name]
  (let [removed (dosync
                 (if (get-in @state [:players name])
                   (do
                     (alter state
                            (fn [s]
                              (-> s
                                  (update :players dissoc name)
                                  (assoc :starting? false :countdown-start nil))))
                     true)
                   false))]
    (when removed
      (broadcast-to-lobby (str name " left the lobby."))
      true)))

(defn- start-countdown! []
  (when (dosync
          (let [s @state]
            (when (and (all-ready? s) (not (:starting? s)))
              (alter state assoc :starting? true :countdown-start (System/currentTimeMillis))
              true)))
    (broadcast-to-lobby "All players are ready! Game will start in 5 seconds...")
    (future
      (Thread/sleep 5000)
      (when-let [to-start (dosync
                           (let [s @state]
                             (if (and (:starting? s) (all-ready? s))
                               (let [players (:players s)]
                                 (alter state assoc :players {} :starting? false :countdown-start nil)
                                 players)
                               (do
                                 (alter state assoc :starting? false :countdown-start nil)
                                 nil))))]
        (doseq [[name _] to-start]
          (when-let [out (@player/streams name)]
            (binding [*out* out]
              (println "Game is starting!")
              (flush))))
        (doseq [[_ {:keys [start]}] to-start]
          (deliver start :start))))))

(defn mark-ready!
  "Toggle a player's readiness. ready? true sets ready, false clears and cancels countdown."
  [name ready?]
  (if (get-in @state [:players name])
    (do
      (dosync
       (alter state
              (fn [s]
                (-> s
                    (assoc-in [:players name :ready?] ready?)
                    (cond-> (not ready?) (assoc :starting? false :countdown-start nil))))))
      (broadcast-to-lobby (str name " is now " (if ready? "READY" "not ready") "."))
      (start-countdown!)
      (if ready? "You are marked ready." "You are marked not ready."))
    "You are not in the lobby."))
