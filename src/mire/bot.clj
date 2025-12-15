(ns mire.bot
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [mire.player :as player]
            [mire.rooms :as rooms]
            [mire.commands :as commands]
            [mire.lobby :as lobby])
  (:import [java.io File]))

(def bot-name "PrologBot")
(def prolog-path "resources/bot_agent.pl")
(def log-file "bot-actions.log")

(defonce state (atom {:active? false
                      :current-room nil
                      :inventory nil
                      :stats nil}))

(defn- log! [& args]
  "Write to bot log file."
  (try
    (spit log-file 
          (str (java.time.Instant/now) " " (str/join " " args) "\n") 
          :append true)
    (catch Exception _)))

(defn- format-atom [s]
  "Format as Prolog atom. Always quote to handle dashes and special chars."
  (let [n (name s)]
    (str "'" (str/replace n "'" "\\'") "'")))

(defn- format-exits [room]
  "Format room exits as Prolog list."
  (let [exits @(:exits room)]
    (str "["
         (str/join "," (map (fn [[dir dest]]
                              (str (format-atom dir) "-" (format-atom dest)))
                            exits))
         "]")))

(defn- format-items [room]
  "Format room items as Prolog list."
  (let [items @(:items room)]
    (str "[" (str/join "," (map format-atom items)) "]")))

(defn- format-mobs [room]
  "Format room mobs as Prolog list."
  (let [mobs @(:mobs room)]
    (str "["
         (str/join ","
                   (map (fn [mob-ref]
                          (let [m @mob-ref]
                            (format "mob(%s,%d,%d,%d)"
                                    (format-atom (:id m))
                                    (int (:hp m))
                                    (int (:max-hp m))
                                    (int (:damage m)))))
                        mobs))
         "]")))

(defn- format-inventory [inv]
  "Format inventory as Prolog list."
  (str "[" (str/join "," (map format-atom (keys inv))) "]"))

(defn- build-prolog-goal [room inv stats]
  "Build the Prolog goal to get an action."
  (let [s @stats
        armor-resist (get-in s [:slots :armor :resist] 0)
        buff-resist (or (:resist_pct s) 0)
        total-resist (min 90 (+ armor-resist buff-resist))]
    (format "sync_state(%s,%s,%s,%s,%s,%d,%d,%d,%d), choose_action(A), action_to_string(A,S), write(S), nl, halt."
            (format-atom (:name room))
            (format-exits room)
            (format-items room)
            (format-mobs room)
            (format-inventory @inv)
            (int (:hp s))
            (int (:max-hp s))
            (int (or (:damage s) 0))
            (int total-resist))))

(defn- query-prolog [room inv stats]
  "Run swipl to get bot action."
  (try
    (let [goal (build-prolog-goal room inv stats)
          _ (log! "Query:" goal)
          pb (ProcessBuilder. ["swipl" "-q" "-l" prolog-path "-g" goal])
          _ (.redirectErrorStream pb true)
          proc (.start pb)
          output (slurp (.getInputStream proc))
          _ (.waitFor proc 5 java.util.concurrent.TimeUnit/SECONDS)
          action (-> output str/trim (str/split #"\n") last str/trim)]
      (log! "Prolog output:" (pr-str output) "-> action:" action)
      (when (and (not (str/blank? action))
                 (not (str/includes? action "ERROR"))
                 (not (str/includes? action "Warning")))
        action))
    (catch Exception e
      (log! "Prolog error:" (.getMessage e))
      nil)))

(defn- human-count []
  (count (remove #(= bot-name %) (keys @player/streams))))

(defn stop! []
  "Stop the bot."
  (let [{:keys [current-room stats]} @state]
    (when current-room
      (dosync
       (when-let [room @current-room]
         (commute (:inhabitants room) disj bot-name))))
    (dosync
     (commute player/streams dissoc bot-name))
    (when stats
      (lobby/unregister-player-stats! bot-name))
    (reset! state {:active? false
                   :current-room nil
                   :inventory nil
                   :stats nil})
    (log! "Bot stopped")))

(defn start! []
  "Start the bot if conditions are met."
  (when (and (not (:active? @state))
             (= (human-count) 1)
             @lobby/game-end-time)
    (when-let [start-room (or (rooms/random-room) (@rooms/rooms :start))]
      (let [current-room (ref start-room)
            inventory (ref {})
            stats (player/init-stats 100 4)]
        (dosync
         (commute (:inhabitants start-room) conj bot-name)
         (commute player/streams assoc bot-name (java.io.StringWriter.)))
        (lobby/register-player-stats! bot-name stats)
        (swap! state assoc
               :active? true
               :current-room current-room
               :inventory inventory
               :stats stats)
        (log! "Bot started in room:" (:name start-room))))))

(defn ensure-bot! []
  "Ensure bot is running if there's exactly one human player."
  (if (and (= (human-count) 1) @lobby/game-end-time)
    (start!)
    (when (:active? @state)
      (stop!))))

(defn- execute-action! [action]
  "Execute an action and update bot state."
  (let [{:keys [current-room inventory stats]} @state
        room @current-room]
    (binding [player/*name* bot-name
              player/*current-room* current-room
              player/*inventory* inventory
              player/*stats* stats
              *out* (java.io.StringWriter.)]
      (let [result (commands/execute action)]
        (when (str/starts-with? action "move")
          (let [new-room @current-room]
            (when (not= (:name new-room) (:name room))
              (log! "Moved to:" (:name new-room)))))
        result))))

(defn tick! []
  "Execute one bot tick."
  (ensure-bot!)
  (when (and (:active? @state)
             (= (human-count) 1)
             @lobby/game-end-time)
    (let [{:keys [current-room inventory stats]} @state]
      (when-let [room @current-room]
        (when (player/alive? stats)
          (when-let [action (query-prolog room inventory stats)]
            (log! "Executing:" action 
                  "| HP:" (:hp @stats) "/" (:max-hp @stats)
                  "| Room:" (:name room)
                  "| Mobs:" (count @(:mobs room))
                  "| Items:" (count @(:items room)))
            (execute-action! action)))))))
