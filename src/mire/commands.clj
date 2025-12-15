(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]
            [mire.mobs :as mobs]
            [mire.items :as items]
            [mire.puzzles :as puzzles]
            [mire.lobby :as lobby]))

(declare show-timer)

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (let [room @player/*current-room*
        exits (->> @(:exits room) keys (map name) (clojure.string/join ", "))
        items (seq @(:items room))
        mobs (seq @(:mobs room))
        puzzle-ref (:puzzle room)
        puzzle (when puzzle-ref @puzzle-ref)
        traders (seq @(:traders room))
        others (disj @(:inhabitants room) player/*name*)]
    (with-out-str
      (println (:desc room))
      (println (format "Exits: %s" exits))
      (when items
        (println (format "You see: %s" (clojure.string/join ", " (map name items)))))
      (when mobs
        (println "Enemies here:")
        (doseq [[idx mob-ref] (map-indexed vector mobs)]
          (let [m @mob-ref]
            (println (format "  %d) %s [HP: %d/%d]" (inc idx) (:name m) (:hp m) (:max-hp m))))))
      (when puzzle
        (println (format "You notice a mysterious inscription: \"%s\"" (:q puzzle)))
        (println "Use: puzzle"))
      (when traders
        (println (format "Traders here: %s" (clojure.string/join ", " (map :name traders)))))
      (when (seq others)
        (println (format "Also here: %s" (clojure.string/join ", " (map name others)))))
      (println)
      (println "Commands:")
      (println " 1)Look   2)Move   3)Grab   4)Inventory")
      (println " 5)Attack 6)Use    7)Equip  8)Trade")
      (println " 9)Stats  *)Puzzle #)Timer  +)LevelUp 0)Quit")
      (println (show-timer)))))

(defn move
  "Move in a direction."
  [direction]
  (dosync
   (let [target-name ((:exits @player/*current-room*) (keyword direction))
         target (@rooms/rooms target-name)
         current-room @player/*current-room*
         mobs-here (seq @(:mobs current-room))]
     (if target
       (do
         
         (when (seq mobs-here)
           (let [max-hp (:max-hp @player/*stats*)
                 flee-damage (max 1 (quot max-hp 10))]
             (player/damage! player/*stats* flee-damage)))
         (move-between-refs player/*name*
                            (:inhabitants current-room)
                            (:inhabitants target))
         (ref-set player/*current-room* target)
         (if (seq mobs-here)
           (with-out-str
             (println "You flee and lose" (max 1 (quot (:max-hp @player/*stats*) 10)) "HP!")
             (println)
             (println (look)))
           (look)))
       (with-out-str (println "You can't go that way."))))))

(defn grab
  "Pick something up."
  [thing]
  (let [room @player/*current-room*
        item-key (keyword thing)]
    (dosync
     (if (rooms/room-contains? room thing)
       (do
         (alter (:items room) disj item-key)
         (alter player/*inventory* #(update % item-key (fnil inc 0)))
         (with-out-str
           (println (format "You picked up %s." thing))))
       (with-out-str
         (println (format "There is no %s here." thing)))))))

(defn discard
  "Put something down that you're carrying."
  [thing]
  (let [room @player/*current-room*
        item-key (keyword thing)]
    (dosync
     (if (player/carrying? thing)
       (do
         (alter player/*inventory*
                (fn [inv]
                  (let [cur (get inv item-key 0)]
                    (cond
                      (> cur 1) (assoc inv item-key (dec cur))
                      (= cur 1) (dissoc inv item-key)
                      :else inv))))
         (alter (:items room) conj item-key)
         (with-out-str
           (println (format "You dropped %s." thing))))
       (with-out-str
         (println (format "You don't have %s." thing)))))))

(defn inventory
  "See what you've got."
  []
  (let [inv-map @player/*inventory*]
    (if (seq inv-map)
      (with-out-str
        (println "You are carrying:")
        (doseq [[k c] (sort-by key inv-map)]
          (println (format "  %s%s"
                           (name k)
                           (if (> c 1) (format " (%d)" c) ""))))))
      "Your inventory is empty."))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (player/carrying? "detector")
    (let [item-key (keyword item)]
      (if-let [room (first (filter #(rooms/room-contains? @(second %) item)
                                   @rooms/rooms))]
        (with-out-str
          (println (format "%s is in %s" item (name (first room)))))
        (with-out-str
          (println (format "%s is not here" item)))))
    (with-out-str
      (println "You don't have a detector!"))))

(defn use-item
  "Use an item in your inventory."
  [item]
  (if-not (player/carrying? item)
    (with-out-str
      (println (format "You don't have %s." item)))
    (let [item-key (keyword item)
          potion (items/get-potion item-key)]
      (cond
        
        (= item-key :weapon-upgrade)
        (do
          (player/remove-item! item-key)
          (player/upgrade-weapon! player/*stats*)
          (with-out-str
            (println "You upgraded your weapon! Damage increased by 2.")
            (println "Your current damage:" (:damage @player/*stats*))))
        
        
        potion
        (do
          (player/remove-item! item-key)
          (if-let [heal (:heal potion)]
            (with-out-str
              (player/heal! player/*stats* heal)
              (println "You drank" (:name potion) "and restored" heal "HP!")
              (println "Your HP:" (:hp @player/*stats*) "/" (:max-hp @player/*stats*)))
            (if-let [res (:resist potion)]
              (with-out-str
                (let [turns (:turns potion 3)]
                  (player/apply-resist! player/*stats* res turns)
                  (println "You drank" (:name potion) "and gained" res "% resistance for" turns "turns!")))
              (with-out-str
                (println "You used" (:name potion) ", but nothing happened.")))))
        
        :else
        (with-out-str
          (println (format "You can't use %s." item)))))))

(defn equip-item
  "Equip a weapon or armor from inventory."
  [item-name]
  (if-not (player/carrying? item-name)
    (with-out-str
      (println (format "You don't have %s in inventory." item-name)))
    (let [weapon (items/get-weapon item-name)
          armor (items/get-armor item-name)]
      (cond
        weapon
        (do
          (player/remove-item! item-name)
          (player/equip-weapon! player/*stats* weapon)
          (with-out-str
            (println (format "You equipped weapon: %s." (:name weapon)))))
        
        armor
        (do
          (player/remove-item! item-name)
          (player/equip-armor! player/*stats* armor)
          (with-out-str
            (println (format "You equipped armor: %s (resistance: %d%%)." (:name armor) (:resist armor)))))
        
        :else
        (with-out-str
          (println "This can't be equipped."))))))

(defn unequip
  "Remove your weapon."
  []
  (player/equip-weapon! player/*stats* nil)
  (with-out-str
    (println "You are now unarmed.")))

(defn display-stats
  "Show player stats."
  []
  (let [s @player/*stats*
        xp-needed (player/xp-for-level (:level s))
        weapon (get-in s [:slots :weapon])
        armor (get-in s [:slots :armor])
        buff-resist (:resist_pct s)
        buff-turns (:resist_turns s)]
    (with-out-str
      (println "=== Stats ===")
      (println "HP:" (:hp s) "/" (:max-hp s))
      (println "Damage:" (:damage s) "(base:" (:base-damage s) ")")
      (println "Weapon:" (if weapon (:name weapon) "Fists"))
      (println "Armor:" (if armor (format "%s (%d%%)" (:name armor) (:resist armor)) "None"))
      (when (pos? buff-resist)
        (println "Resistance buff:" buff-resist "% for" (max 0 buff-turns) "more turns"))
      (println "XP:" (:xp s) "/" xp-needed)
      (println "Level:" (:level s))
      (println "Gold:" (:gold s))
      (when (:pending-levelup s)
        (println)
        (println "*** LEVEL UP! Use 'levelup' command ***")))))

(defn levelup-choice
  "Apply level-up bonus based on choice."
  [choice]
  (let [s @player/*stats*]
    (if-not (:pending-levelup s)
      (with-out-str
        (println "You have no available level-ups."))
      (case choice
        "1" (do (player/apply-level-up! player/*stats* :damage)
                (with-out-str
                  (println "You chose +2 damage!")
                  (println "Your new base damage:" (:base-damage @player/*stats*))))
        "2" (do (player/apply-level-up! player/*stats* :hp)
                (with-out-str
                  (println "You chose +15 max HP!")
                  (println "Your new HP:" (:hp @player/*stats*) "/" (:max-hp @player/*stats*))))
        (with-out-str
          (println "Choose upgrade:")
          (println "1) +2 damage")
          (println "2) +15 max HP")
          (println "Use: levelup 1 or levelup 2"))))))

(defn display-help
  "Get help."
  []
  (with-out-str
    (println "Explore the dungeon and have fun!")))

(defn say
  "Broadcast a message to the current room."
  [& words]
  (let [msg (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*) player/*name*)]
      (if-let [output (get @player/streams inhabitant)]
        (binding [*out* output]
          (println player/*name* "says:" msg)
          (flush))))
    (with-out-str
      (println "You say:" msg))))

(defn attack-mob
  "Attack an enemy in the current room. All mobs attack back."
  [& args]
  (let [room @player/*current-room*
        mob-list (vec @(:mobs room))]
    (if (empty? mob-list)
      (with-out-str (println "There are no enemies here."))
      (let [target-arg (first args)
            idx (when target-arg (try (Integer/parseInt target-arg) (catch Exception _ nil)))
            target-mob (cond
                         (nil? target-arg) (first mob-list)
                         (and idx (<= 1 idx) (<= idx (count mob-list))) (mob-list (dec idx))
                         (and idx (not (<= 1 idx (count mob-list)))) :invalid-index
                         :else (mobs/find-mob-in-room room (str/lower-case target-arg)))]
        (cond
          (= target-mob :invalid-index)
          (with-out-str (println "Invalid target number."))

          (nil? target-mob)
          (with-out-str (println "That mob is not here!"))

          :else
          (let [mob target-mob]
            (mobs/player-attack-mob! player/*stats* mob)
            (let [m @mob]
              (if (<= (:hp m) 0)
                (let [gold-reward (or (:gold m) 10)
                      xp-reward (or (:xp m) 10)
                      potion-drop (if (< (rand) 0.65) :hp-small :hp-medium)
                      potion-name (if (= potion-drop :hp-small) "Small HP Potion" "Medium HP Potion")]
                  (player/add-gold! player/*stats* gold-reward)
                  (player/add-xp! player/*stats* xp-reward)
                  (player/add-item! potion-drop)
                  (mobs/remove-mob-from-room! mob room)
                  (let [remaining-mobs (seq @(:mobs room))
                        counter (when remaining-mobs (mobs/all-mobs-attack! room player/*stats*))]
                    (with-out-str
                      (println "You defeated" (:name m) "! Got" gold-reward "gold and" xp-reward "XP!")
                      (println "Dropped:" potion-name "!")
                      (when counter
                        (println counter))
                      (if (player/alive? player/*stats*)
                        (println "Your HP:" (:hp @player/*stats*) "/" (:max-hp @player/*stats*))
                        (println "*** YOU DIED ***")))))
                (let [counter (mobs/all-mobs-attack! room player/*stats*)]
                  (with-out-str
                    (println "You hit" (:name m) "! [HP:" (:hp m) "/" (:max-hp m) "]")
                    (println "All enemies attack!")
                    (println counter)
                    (if (player/alive? player/*stats*)
                      (println "Your HP:" (:hp @player/*stats*) "/" (:max-hp @player/*stats*))
                      (println "*** YOU DIED ***"))))))))))))
(defn solve-puzzle
  "Show or attempt to solve a puzzle in the current room."
  [& args]
  (let [room @player/*current-room*
        puzzle-ref (:puzzle room)
        puzzle (when puzzle-ref @puzzle-ref)]
    (if-not puzzle
      (with-out-str
        (println "There is no puzzle here."))
      (if (empty? args)
        (with-out-str
          (println (format "Puzzle: \"%s\"" (:q puzzle)))
          (println "Choices:")
          (doseq [[i c] (map-indexed vector (:choices puzzle))]
            (println (format "%d) %s" i c)))
          (println "Use: solve <number>"))
        (let [choice-idx (try (Integer/parseInt (first args)) (catch Exception _ nil))]
          (if (nil? choice-idx)
            (with-out-str
              (println "Enter a number (0, 1, or 2) to answer."))
            (if (<= choice-idx (dec (count (:choices puzzle))))
              (let [answer-correct (= choice-idx (:answer puzzle))]
                (dosync (ref-set puzzle-ref nil))
                (if answer-correct
                  (let [gold-reward (+ 10 (rand-int 16))
                        xp-reward (:xp puzzle)]
                    (player/add-gold! player/*stats* gold-reward)
                    (player/add-xp! player/*stats* xp-reward)
                    (with-out-str
                      (println "Correct! You solved the puzzle!")
                      (println "Got" xp-reward "XP and" gold-reward "gold!")))
                  (with-out-str
                    (println "Wrong. The correct answer was:" ((:choices puzzle) (:answer puzzle))))))
              (with-out-str
                (println "Invalid choice number.")))))))))


(def item-prices
  {:hp-small 20
   :hp-medium 40
   :resist 55
   :dagger 45
   :club 35
   :sword 85
   :axe 105
   :spear 125
   :mace 145
   :warhammer 175
   :crossbow 160
   :leather 70
   :scale 110
   :chain 145
   :plate 200
   :weapon-upgrade 90})

(def trader-stock
  {:general [:hp-small :hp-medium :resist :dagger :club :sword :leather :weapon-upgrade]
   :scavenger [:hp-small :hp-medium :resist :dagger :club :crossbow :leather]
   :blacksmith [:sword :axe :spear :mace :warhammer :crossbow :scale :chain :plate :weapon-upgrade]
   :alchemist [:hp-small :hp-medium :resist :weapon-upgrade]
   :fortune [:resist :hp-medium :weapon-upgrade :dagger]})

(defn- item-name [id]
  (or (:name (items/get-weapon id))
      (:name (items/get-armor id))
      (:name (items/get-potion id))
      (when (= id :weapon-upgrade) (:name items/upgrade-kit))
      (name id)))

(defn- item-info [id]
  (when-let [price (get item-prices id)]
    {:id id
     :name (item-name id)
     :price price}))

(defn- trader-items [trader]
  (let [ids (get trader-stock (or (:type trader) :general)
                   (get trader-stock :general))]
    (->> ids
         (map item-info)
         (remove nil?))))

(defn- sell-price [price]
  (max 1 (int (Math/ceil (* price 0.5)))))

(defn- sellable-items [inv-map]
  (->> inv-map
       (map (fn [[id qty]]
              (when-let [info (item-info id)]
                (assoc info :qty qty :sell-price (sell-price (:price info))))))
       (remove nil?)
       (sort-by :name)))

(defn trade
  "Trade with a merchant in the current room."
  [& args]
  (let [room @player/*current-room*
        traders (seq @(:traders room))]
    (if-not traders
      (with-out-str
        (println "There is no trader here."))
      (let [trader (first traders)
            stock (vec (trader-items trader))
            inv-map @player/*inventory*
            sellable (vec (sellable-items inv-map))
            trader-role (name (or (:type trader) :general))
            header (format "%s (%s) welcomes you. Gold: %d"
                           (or (:name trader) "Trader")
                           trader-role
                           (player/get-gold player/*stats*))]
        (if (empty? args)
          (with-out-str
            (println header)
            (when (seq stock)
              (println "Buy:")
              (doseq [[i itm] (map-indexed vector stock)]
                (println (format "%d) %s - %d gold" (inc i) (:name itm) (:price itm)))))
            (when (seq sellable)
              (println "Sell:")
              (doseq [[i itm] (map-indexed vector sellable)]
                (println (format "%d) %s (%d) - %d gold"
                                 (inc i) (:name itm) (:qty itm) (:sell-price itm)))))
            (println "Use: trade <number> to buy or trade sell <number> to sell."))
          (let [[action idx-str] (if (and (seq args) (#{"buy" "sell"} (first args)))
                                   [(first args) (second args)]
                                   ["buy" (first args)])
                idx (try (Integer/parseInt (or idx-str "")) (catch Exception _ nil))]
            (cond
              (nil? idx)
              (with-out-str
                (println "Enter item number."))

              (= action "sell")
              (cond
                (empty? sellable) (with-out-str
                                    (println "You have nothing the trader wants."))
                (not (<= 1 idx (count sellable))) (with-out-str
                                                    (println "Invalid item number."))
                :else (let [{:keys [id name sell-price]} (sellable (dec idx))]
                        (if (player/remove-item! id)
                          (do
                            (player/add-gold! player/*stats* sell-price)
                            (with-out-str
                              (println (format "Sold %s for %d gold." name sell-price))))
                          (with-out-str
                            (println "You don't have that item anymore.")))))

              :else
              (cond
                (empty? stock) (with-out-str
                                 (println (format "%s has nothing to sell right now."
                                                  (or (:name trader) "Trader"))))
                (not (<= 1 idx (count stock))) (with-out-str
                                                 (println "Invalid item number."))
                :else (let [{:keys [id name price]} (stock (dec idx))]
                        (if (player/spend-gold! player/*stats* price)
                          (do
                            (player/add-item! id)
                            (with-out-str
                              (println (format "You bought %s for %d gold!" name price))))
                          (with-out-str
                            (println (format "Not enough gold! Need %d, you have %d."
                                             price
                                             (player/get-gold player/*stats*))))))))))))))

(defn show-timer
  "Show remaining game time."
  []
  (if-let [end-time @lobby/game-end-time]
    (let [remaining (- end-time (System/currentTimeMillis))
          minutes (quot remaining 60000)
          seconds (quot (mod remaining 60000) 1000)]
      (if (pos? remaining)
        (with-out-str
          (print (format "Time remaining: %d:%02d" minutes seconds)))
        (with-out-str
          (println "Time's up!"))))
    (with-out-str
      (println "Timer not started."))))

(def commands
  {"move" move
   "north" (fn [] (move "north"))
   "south" (fn [] (move "south"))
   "east" (fn [] (move "east"))
   "west" (fn [] (move "west"))
   "look" look
   "grab" grab
   "take" grab
   "drop" discard
   "discard" discard
   "inventory" inventory
   "detect" detect
   "use" use-item
   "equip" equip-item
   "unequip" unequip
   "stats" display-stats
   "help" display-help
   "say" say
   "attack" attack-mob
   "solve" solve-puzzle
   "trade" trade
   "timer" show-timer
   "levelup" levelup-choice})

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try
    (let [[command & args] (.split input " +")]
      (apply (commands command) args))
    (catch Exception e
      (.printStackTrace e (new java.io.PrintWriter *err*))
      "You can't do that!")))
