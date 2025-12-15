(ns mire.server
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]
            [mire.items :as items]
            [mire.lobby :as lobby]
            [mire.bot :as bot]))

(defn clear-screen []
  (print "\u001b[2J\u001b[H")
  (flush))

(defn- cleanup []
  (lobby/leave! player/*name*)
  (lobby/unregister-player-stats! player/*name*)
  (when-let [room @player/*current-room*]
    (doseq [item (player/inventory-items)]
      (commands/discard item))
    (dosync
     (commute (:inhabitants room) disj player/*name*)))
  (dosync
   (commute player/streams dissoc player/*name*))
  (bot/ensure-bot!))

(defn- get-unique-player-name [name]
  (if (@player/streams name)
    (do (print "This name is taken, try another: ")
        (flush)
        (recur (read-line)))
    name))

(defn- check-game-time []
  (when-let [end-time @lobby/game-end-time]
    (<= end-time (System/currentTimeMillis))))

(defn- sanitize-name [n]
  (let [clean (-> (or n "")
                  str/trim
                  (str/replace #"[^A-Za-z0-9 _\-]" ""))]
    (cond
      (str/blank? clean) "Player"
      (> (count clean) 20) (subs clean 0 20)
      :else clean)))

(defn- show-game-over []
  
  (Thread/sleep 500)
  (locking lobby/game-end-time
    (when @lobby/game-end-time
      (lobby/broadcast-game-over!)
      (reset! lobby/game-end-time nil)))
  (println "\n========================================")
  (println "         YOUR PERSONAL RESULTS")
  (println "========================================")
  (let [s @player/*stats*]
    (println (str "  Level: " (:level s)))
    (println (str "  XP: " (:xp s)))
    (println (str "  Gold: " (:gold s)))
    (println (str "  HP: " (:hp s) "/" (:max-hp s))))
  (println "========================================")
  (println "\nGame over. Type 'quit' to exit.")
  (flush)
  (loop [line (read-line)]
    (when line
      (if (= (str/lower-case (str/trim (or line ""))) "quit")
        (println "Goodbye!")
        (do
          (println "Type 'quit' to exit.")
          (flush)
          (recur (read-line)))))))

(defn- process-command [cmd menu]
  (clear-screen)
  (cond
    (= cmd "quit")
    (do (println "Goodbye!") (flush) :quit)

    (= cmd "look")
    (println (commands/execute "look"))

    (= cmd "inventory")
    (println (commands/execute "inventory"))

    (= cmd "stats")
    (println (commands/execute "stats"))

    (= cmd "timer")
    (println (commands/show-timer))

    (= cmd "levelup")
    (let [s @player/*stats*]
      (if-not (:pending-levelup s)
        (println "You have no available level-ups.")
        (do
          (println "=== LEVEL UP! ===")
          (println "Choose upgrade:")
          (println "1) +2 damage")
          (println "2) +15 max HP")
          (print "Your choice: ") (flush)
          (let [choice (str/trim (or (read-line) ""))]
            (println (commands/execute (str "levelup " choice)))))))

    (= cmd "help")
    (println (commands/execute "help"))

    (= cmd "move")
    (let [exits @(:exits @player/*current-room*)]
      (if (empty? exits)
        (println "There are no exits here!")
        (do
          (println "\nAvailable directions:")
          (doseq [[idx [dir _]] (map-indexed vector (sort exits))]
            (println (format "  %d) %s" (inc idx) (name dir))))
          (print "Choose direction: ") (flush)
          (let [sel-str (str/trim (or (read-line) ""))
                sel (try (Integer/parseInt sel-str) (catch Exception _ nil))
                dir-choices (vec (map first (sort exits)))]
            (if (and sel (<= 1 sel) (<= sel (count dir-choices)))
              (do
                (println)
                (println (commands/execute (str "move " (name (dir-choices (dec sel)))))))
              (println "Invalid choice."))))))

    (= cmd "attack")
    (let [mobs-here (seq @(:mobs @player/*current-room*))]
      (if (empty? mobs-here)
        (println "No enemies here.")
        (do
          (println "Enemies in room:")
          (doseq [[idx mob-atom] (map-indexed vector mobs-here)]
            (let [m @mob-atom]
              (println (format "  %d) %s [HP: %d/%d]" (inc idx) (:name m) (:hp m) (:max-hp m)))))
          (print "Choose target (or Enter for first): ") (flush)
          (let [sel-str (str/trim (or (read-line) ""))
                cmd (if (str/blank? sel-str) "attack" (str "attack " sel-str))
                result (commands/execute cmd)]
            (println result)
            (when-not (player/alive? player/*stats*)
              (println "\n*** GAME OVER ***"))))))

    (= cmd "use")
    (let [inv-map @player/*inventory*
          usable (->> inv-map
                      keys
                      (filter #(or (items/get-potion %) (= % :weapon-upgrade))))] 
      (if (empty? usable)
        (println "You have no items to use.")
        (do
          (println "Available items:")
          (doseq [[idx item] (map-indexed vector usable)]
            (let [p (items/get-potion item)
                  qty (get inv-map item 0)
                  item-name (cond
                              p (:name p)
                              (= item :weapon-upgrade) "Weapon Upgrade (+2 damage)"
                              :else (name item))]
              (println (format "  %d) %s%s - %s"
                               (inc idx)
                               (name item)
                               (if (> qty 1) (format " (%d)" qty) "")
                               item-name))))
          (print "Choose item: ") (flush)
          (let [sel-str (str/trim (or (read-line) ""))
                sel (try (Integer/parseInt sel-str) (catch Exception _ nil))
                item-choices (vec usable)]
            (if (and sel (<= 1 sel) (<= sel (count item-choices)))
              (println (commands/execute (str "use " (name (item-choices (dec sel))))))
              (println "Invalid choice."))))))

    (= cmd "equip")
    (let [inv-map @player/*inventory*
          inv (keys inv-map)
          weapons (filter #(items/get-weapon %) inv)
          armors (filter #(items/get-armor %) inv)
          equippable (concat weapons armors)]
      (if (empty? equippable)
        (println "You have no equipment in inventory.")
        (do
          (println "Equipment in inventory:")
          (doseq [[idx item] (map-indexed vector equippable)]
            (let [weapon (items/get-weapon item)
                  armor (items/get-armor item)
                  qty (get inv-map item 0)]
              (cond
                weapon (println (format "  %d) %s%s [weapon, damage: %d]"
                                        (inc idx) (:name weapon)
                                        (if (> qty 1) (format " (%d)" qty) "")
                                        (:damage weapon)))
                armor (println (format "  %d) %s%s [armor, resist: %d%%]"
                                       (inc idx) (:name armor)
                                       (if (> qty 1) (format " (%d)" qty) "")
                                       (:resist armor))))))
          (print "Choose equipment: ") (flush)
          (let [sel-str (str/trim (or (read-line) ""))
                sel (try (Integer/parseInt sel-str) (catch Exception _ nil))
                equip-choices (vec equippable)]
            (if (and sel (<= 1 sel) (<= sel (count equip-choices)))
              (println (commands/execute (str "equip " (name (equip-choices (dec sel))))))
              (println "Invalid choice."))))))

    (= cmd "grab")
    (let [items @(:items @player/*current-room*)]
      (if (empty? items)
        (println "Nothing to pick up here.")
        (do
          (println "Items in room:")
          (doseq [[idx item] (map-indexed vector (sort items))]
            (println (format "  %d) %s" (inc idx) (name item))))
          (print "Choose item: ") (flush)
          (let [sel-str (str/trim (or (read-line) ""))
                sel (try (Integer/parseInt sel-str) (catch Exception _ nil))
                item-choices (vec (sort items))]
            (if (and sel (<= 1 sel) (<= sel (count item-choices)))
              (println (commands/execute (str "grab " (name (item-choices (dec sel))))))
              (println "Invalid choice."))))))

    (= cmd "trade")
    (let [traders (when-let [t (:traders @player/*current-room*)] @t)]
      (if (or (nil? traders) (empty? traders))
        (println "No trader here.")
        (do
          (println (commands/execute "trade"))
          (print "Enter 'buy <#>' or 'sell <#>' (or Enter to cancel): ") (flush)
          (let [sel-str (str/trim (or (read-line) ""))]
            (when-not (str/blank? sel-str)
              (println (commands/execute (str "trade " sel-str))))))))

    (= cmd "puzzle")
    (let [puzzle-ref (:puzzle @player/*current-room*)
          puzzle (when puzzle-ref @puzzle-ref)]
      (if puzzle
        (do
          (println (commands/execute "solve"))
          (print "Your answer (0, 1, or 2): ") (flush)
          (let [ans (str/trim (or (read-line) ""))]
            (println (commands/execute (str "solve " ans)))))
        (println "No puzzle here.")))

    :else
    (println (commands/execute cmd)))
  nil
)

(defn- mire-handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]

    (print "\nWhat is your name? ") (flush)
    (binding [player/*name* (get-unique-player-name (sanitize-name (read-line)))
              player/*current-room* (ref nil)
              player/*inventory* (ref {})
              player/*stats* (player/init-stats 100 4)]
      (try
        (dosync
         (commute player/streams assoc player/*name* *out*))
        (bot/ensure-bot!)

        (let [start-signal (lobby/register-player! player/*name*)
              poller (let [buf (StringBuilder.)]
                       (fn []
                         (when (.ready *in*)
                           (loop []
                             (let [ch (.read *in*)]
                               (cond
                                 (= ch -1) {:eof true}
                                 (or (= ch 10) (= ch 13))
                                 (let [s (.toString buf)]
                                   (.setLength buf 0)
                                   {:line s})
                                 :else (do (.append buf (char ch))
                                           (if (.ready *in*) (recur) nil))))))))]
          (println "\nWelcome to LIA`S DUNGEON," player/*name* "!")
          (println "You are in the lobby. Commands: ready, unready, status, quit.")
          (println (lobby/status))
          (print lobby/prompt) (flush)
          (loop []
            (cond
              (realized? start-signal)
              nil

              :else
              (let [res (poller)]
                (cond
                  (nil? res)
                  (do (Thread/sleep 100) (recur))

                  (:eof res)
                  (do (println "Goodbye!") (flush))

                  :else
                  (let [line (str/trim (or (:line res) ""))
                        cmd (str/lower-case line)]
                    (when-not (empty? cmd)
                      (case cmd
                        "ready" (println (lobby/mark-ready! player/*name* true))
                        "unready" (println (lobby/mark-ready! player/*name* false))
                        "status" (println (lobby/status))
                        "help" (println "Lobby commands: ready, unready, status, quit.")
                        "quit" (do (println "Goodbye!") (flush))
                        (println "Unknown command. Try: ready, unready, status, quit.")))
                    (when-not (#{"quit"} cmd)
                      (when-not (:starting? @lobby/state)
                        (print lobby/prompt) (flush))
                      (recur)))))))

          (when (realized? start-signal)
            (lobby/start-game-timer!)
            
            
            (lobby/register-player-stats! player/*name* player/*stats*)
            
            (let [start-room (or (rooms/random-room) (@rooms/rooms :start))]
              (dosync
               (ref-set (:items start-room) #{})
               (ref-set player/*current-room* start-room)
               (commute (:inhabitants @player/*current-room*) conj player/*name*))
              (clear-screen)
              (println "\nYou start the game with bare fists (damage: 4).")
              (println "Find weapons and armor in the dungeon or buy from a trader!")
              (println)
              (println (commands/look)))

            (let [menu {"1" "look"
                        "2" "move"
                        "3" "grab"
                        "4" "inventory"
                        "5" "attack"
                        "6" "use"
                        "7" "equip"
                        "8" "trade"
                        "9" "stats"
                        "*" "puzzle"
                        "#" "timer"
                        "+" "levelup"
                        "0" "quit"}]
              (print player/prompt) (flush)

              (loop [input (read-line)]
                (cond
                  (nil? input)
                  nil
                  
                  (not (player/alive? player/*stats*))
                  (do
                    (println "\n*** YOU DIED ***")
                    (show-game-over))
                  
                  (check-game-time)
                  (show-game-over)
                  
                  :else
                  (let [trim (str/trim (or input ""))
                        cmd (get menu trim trim)
                        result (process-command cmd menu)]
                    (when (not= result :quit)
                      (bot/tick!)
                      (when (:pending-levelup @player/*stats*)
                        (println "\n*** LEVEL UP! Press '+' to choose upgrade ***"))
                      (print player/prompt) (flush)
                      (recur (read-line)))))))))
        (finally (cleanup))))))

(defn -main
  ([port dir use-procedural?]
   ;; Ensure CRLF newlines for telnet clients on Windows terminals.
   (System/setProperty "line.separator" "\r\n")
   (rooms/add-rooms dir use-procedural?)
   (defonce server (socket/create-server (Integer. port) mire-handle-client))
   (println "Launching Mire server on port" port)
   (when use-procedural?
     (println "Procedural dungeon generation enabled")))
  ([port dir] (-main port dir true))
  ([port] (-main port "resources/rooms" true))
  ([] (-main 3333)))
