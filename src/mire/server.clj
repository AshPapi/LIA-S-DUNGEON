(ns mire.server
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]
            [mire.items :as items]
            [mire.lobby :as lobby]))

(defn- cleanup []
  (lobby/leave! player/*name*)
  (when-let [room @player/*current-room*]
    (doseq [item @player/*inventory*]
      (commands/discard item))
    (dosync
     (commute (:inhabitants room) disj player/*name*)))
  (dosync
   (commute player/streams dissoc player/*name*)))

(defn- get-unique-player-name [name]
  (if (@player/streams name)
    (do (print "This name is taken, try another: ")
        (flush)
        (recur (read-line)))
    name))

(defn- process-command [cmd menu]
  (cond
    (= cmd "quit")
    (do (println "Goodbye!") (flush) :quit)

    (= cmd "look")
    (println (commands/execute "look"))

    (= cmd "inventory")
    (println (commands/execute "inventory"))

    (= cmd "stats")
    (println (commands/execute "stats"))

    (= cmd "help")
    (println (commands/execute "help"))

    (= cmd "move")
    (let [exits @(:exits @player/*current-room*)]
      (if (empty? exits)
        (println "There are no exits here!")
        (do
          (println "Available directions:")
          (doseq [[idx [dir _]] (map-indexed vector (sort exits))]
            (println (str (inc idx) ") " (name dir))))
          (print "Choose direction: ") (flush)
          (let [sel-str (str/trim (read-line))
                sel (try (Integer/parseInt sel-str) (catch Exception _ nil))
                dir-choices (vec (map first (sort exits)))]
            (if (and sel (<= 1 sel) (<= sel (count dir-choices)))
              (println (commands/execute (str "move " (name (dir-choices (dec sel))))))
              (println "Invalid choice."))))))

    (= cmd "attack")
    (let [mobs-here (seq @(:mobs @player/*current-room*))]
      (if (empty? mobs-here)
        (println "No enemies here.")
        (do
          (println "Enemies in room:")
          (doseq [[idx mob-atom] (map-indexed vector mobs-here)]
            (let [m @mob-atom]
              (println (str (inc idx) ") " (:name m) " [HP: " (:hp m) "/" (:max-hp m) "]"))))
          (print "Choose target (or Enter for first): ") (flush)
          (let [sel-str (str/trim (read-line))
                result (commands/execute "attack")]
            (println result)
            (when-not (player/alive? player/*stats*)
              (println "\n*** GAME OVER ***"))))))

    (= cmd "use")
    (let [inv (seq @player/*inventory*)
          usable (filter #(or (items/get-potion %) (= % :weapon-upgrade)) inv)]
      (if (empty? usable)
        (println "You have no items to use.")
        (do
          (println "Available items:")
          (doseq [[idx item] (map-indexed vector usable)]
            (let [p (items/get-potion item)
                  item-name (cond
                              p (:name p)
                              (= item :weapon-upgrade) "Weapon Upgrade (+2 damage)"
                              :else (name item))]
              (println (str (inc idx) ") " (name item) " - " item-name))))
          (print "Choose item: ") (flush)
          (let [sel-str (str/trim (read-line))
                sel (try (Integer/parseInt sel-str) (catch Exception _ nil))
                item-choices (vec usable)]
            (if (and sel (<= 1 sel) (<= sel (count item-choices)))
              (println (commands/execute (str "use " (name (item-choices (dec sel))))))
              (println "Invalid choice."))))))

    (= cmd "equip")
    (let [inv (seq @player/*inventory*)
          weapons (filter #(items/get-weapon %) inv)
          armors (filter #(items/get-armor %) inv)
          equippable (concat weapons armors)]
      (if (empty? equippable)
        (println "You have no equipment in inventory.")
        (do
          (println "Equipment in inventory:")
          (doseq [[idx item] (map-indexed vector equippable)]
            (let [weapon (items/get-weapon item)
                  armor (items/get-armor item)]
              (cond
                weapon (println (str (inc idx) ") " (:name weapon) " [weapon, damage: " (:damage weapon) "]"))
                armor (println (str (inc idx) ") " (:name armor) " [armor, resist: " (:resist armor) "%]")))))
          (print "Choose equipment: ") (flush)
          (let [sel-str (str/trim (read-line))
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
            (println (str (inc idx) ") " (name item))))
          (print "Choose item: ") (flush)
          (let [sel-str (str/trim (read-line))
                sel (try (Integer/parseInt sel-str) (catch Exception _ nil))
                item-choices (vec (sort items))]
            (if (and sel (<= 1 sel) (<= sel (count item-choices)))
              (println (commands/execute (str "grab " (name (item-choices (dec sel))))))
              (println "Invalid choice."))))))

    (= cmd "puzzle")
    (let [puzzle-ref (:puzzle @player/*current-room*)
          puzzle (when puzzle-ref @puzzle-ref)]
      (if puzzle
        (do
          (println (commands/execute "solve"))
          (print "Your answer (0, 1, or 2): ") (flush)
          (let [ans (str/trim (read-line))]
            (println (commands/execute (str "solve " ans)))))
        (println "No puzzle here.")))

    :else
    (println (commands/execute cmd)))
  nil)

(defn- mire-handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]

    (print "\nWhat is your name? ") (flush)
    (binding [player/*name* (get-unique-player-name (read-line))
              player/*current-room* (ref nil)
              player/*inventory* (ref #{})
              player/*stats* (player/init-stats 100 4)]
      (try
        (dosync
         (commute player/streams assoc player/*name* *out*))

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
          (println "\nWelcome to Mire," player/*name* "!")
          (println "You are in the lobby. Commands: ready, unready, status, quit.")
          (println (lobby/status))
          (print lobby/prompt) (flush)
          (loop []
            (cond
              (realized? start-signal)
              (println "\nAll players ready! Starting dungeon...")

              :else
              (let [res (poller)]
                (cond
                  (nil? res)
                  (do (Thread/sleep 100) (recur))

                  (:eof res)
                  (do (println "Goodbye!") (flush))

                  :else
                  (let [line (str/trim (:line res))
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
                      (print lobby/prompt) (flush)
                      (recur)))))))

          (when (realized? start-signal)
            (let [start-room (@rooms/rooms :start)]
              (dosync
               (ref-set player/*current-room* start-room)
               (commute (:inhabitants @player/*current-room*) conj player/*name*))
              (println (commands/look)))
            
            (println "\nYou start the game with bare fists (damage: 4).")
            (println "Find weapons and armor in the dungeon as you explore!")

            (let [menu {"1" "look"
                        "2" "move"
                        "3" "grab"
                        "4" "inventory"
                        "5" "attack"
                        "6" "use"
                        "7" "equip"
                        "9" "stats"
                        "*" "puzzle"
                        "0" "quit"}]
              (println "\nCommands: 1)Look 2)Move 3)Grab 4)Inventory 5)Attack 6)Use 7)Equip 9)Stats *)Puzzle 0)Quit")
              (print player/prompt) (flush)

              (loop [input (read-line)]
                (cond
                  (nil? input)
                  nil
                  
                  (not (player/alive? player/*stats*))
                  (println "\n*** YOU DIED ***")
                  
                  :else
                  (let [trim (str/trim input)
                        cmd (get menu trim trim)
                        result (process-command cmd menu)]
                    (when (not= result :quit)
                      (print player/prompt) (flush)
                      (recur (read-line)))))))))
        (finally (cleanup))))))

(defn -main
  ([port dir use-procedural?]
   (rooms/add-rooms dir use-procedural?)
   (defonce server (socket/create-server (Integer. port) mire-handle-client))
   (println "Launching Mire server on port" port)
   (when use-procedural?
     (println "Procedural dungeon generation enabled")))
  ([port dir] (-main port dir true))
  ([port] (-main port "resources/rooms" true))
  ([] (-main 3333)))
