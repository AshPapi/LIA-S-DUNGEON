(ns mire.lobby
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [mire.player :as player]))

;; Lobby state tracks connected players before the game starts.
;; Each player entry holds {:ready? boolean :start promise}

(def prompt "lobby> ")

(defonce state (ref {:players {} :starting? false :countdown-start nil}))

;; Game timer - 10 minutes from start
(def game-end-time (atom nil))
(def game-duration-ms (* 2 60 1000)) ;; 2 minutes for testing

;; Store player scores for end game
(def player-scores (atom {}))

;; Store refs to player stats for live tracking
(def player-stats-refs (atom {}))

(defn register-player-stats! [name stats-ref]
  "Register a player's stats ref for live tracking."
  (swap! player-stats-refs assoc name stats-ref))

(defn unregister-player-stats! [name]
  "Unregister a player's stats ref."
  (swap! player-stats-refs dissoc name))

(defn collect-all-scores! []
  "Collect scores from all registered players."
  (doseq [[name stats-ref] @player-stats-refs]
    (when stats-ref
      (try
        (let [stats @stats-ref]
          (swap! player-scores assoc name {:xp (:xp stats)
                                           :level (:level stats)
                                           :gold (:gold stats)
                                           :hp (:hp stats)
                                           :max-hp (:max-hp stats)
                                           :alive (> (:hp stats) 0)}))
        (catch Exception _)))))

(defn start-game-timer! []
  (reset! game-end-time (+ (System/currentTimeMillis) game-duration-ms))
  (reset! player-scores {}))

(defn record-player-score! [name stats]
  "Record a player's final score with alive status."
  (swap! player-scores assoc name {:xp (:xp stats)
                                   :level (:level stats)
                                   :gold (:gold stats)
                                   :hp (:hp stats)
                                   :max-hp (:max-hp stats)
                                   :alive (> (:hp stats) 0)}))

(defn get-winner []
  "Get the winner based on XP, then level, then gold. Only alive players can win."
  (when (seq @player-scores)
    (let [alive-players (filter (fn [[_ s]] (:alive s)) @player-scores)
          sorted (sort-by (fn [[_ s]] [(- (:xp s)) (- (:level s)) (- (:gold s))]) alive-players)]
      (first sorted))))

(defn get-all-scores []
  "Get all scores sorted by XP. Dead players are shown at the bottom."
  (let [alive (filter (fn [[_ s]] (:alive s)) @player-scores)
        dead (filter (fn [[_ s]] (not (:alive s))) @player-scores)
        sorted-alive (sort-by (fn [[_ s]] [(- (:xp s)) (- (:level s)) (- (:gold s))]) alive)
        sorted-dead (sort-by (fn [[_ s]] [(- (:xp s)) (- (:level s)) (- (:gold s))]) dead)]
    (concat sorted-alive sorted-dead)))

(defn broadcast-game-over! []
  "Broadcast game over message with winner to all players."
  ;; First collect all scores from all players
  (collect-all-scores!)
  (let [scores (get-all-scores)
        winner (get-winner)
        alive-count (count (filter (fn [[_ s]] (:alive s)) @player-scores))
        msg (str "\n========================================\n"
                 "         *** TIME'S UP! ***\n"
                 "========================================\n\n"
                 (if winner
                   (str "🏆 WINNER: " (first winner) " 🏆\n"
                        "   XP: " (:xp (second winner)) 
                        " | Level: " (:level (second winner))
                        " | Gold: " (:gold (second winner)) "\n\n")
                   (if (zero? alive-count)
                     "All players died! No winner.\n\n"
                     "No participants.\n\n"))
                 (when (seq scores)
                   (str "--- LEADERBOARD ---\n"
                        (clojure.string/join "\n" 
                          (map-indexed (fn [idx [name s]]
                                         (str (inc idx) ". " name 
                                              (if (:alive s) "" " [DEAD]")
                                              " - XP: " (:xp s)
                                              ", Lvl: " (:level s)
                                              ", Gold: " (:gold s)))
                                       scores))
                        "\n"))
                 "\n========================================\n"
                 "      Thanks for playing!\n"
                 "========================================\n")]
    (doseq [[name _] @player/streams]
      (when-let [out (@player/streams name)]
        (try
          (binding [*out* out]
            (println msg)
            (flush))
          (catch Exception _))))))

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
        ;; Send start notice directly to players (state is cleared now).
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
