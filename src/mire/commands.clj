(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]
            [mire.mobs :as mobs]
            [mire.items :as items]
            [mire.puzzles :as puzzles]
            [mire.lobby :as lobby]))

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
    (str (:desc room)
         "\nExits: " exits "\n"
         (when items (str "You see: " (clojure.string/join ", " (map name items)) "\n"))
         (when mobs (str "Enemies here: " 
                         (clojure.string/join ", " 
                           (map #(let [m @%] (str (:name m) " [HP: " (:hp m) "/" (:max-hp m) "]")) mobs)) 
                         "\n"))
         (when puzzle (str "You notice a mysterious inscription: \"" (:q puzzle) "\"\nUse: puzzle\n"))
         (when traders (str "Traders here: " (clojure.string/join ", " (map :name traders)) "\n"))
         (when (seq others) (str "Also here: " (clojure.string/join ", " (map name others)) "\n"))
         "\nCommands: 1)Look 2)Move 3)Grab 4)Inventory 5)Attack 6)Use 7)Equip 8)Trade 9)Stats *)Puzzle +)LevelUp 0)Quit")))

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
         ;; If fleeing from mobs, lose 10% max HP
         (when (seq mobs-here)
           (let [max-hp (:max-hp @player/*stats*)
                 flee-damage (max 1 (quot max-hp 10))]
             (player/damage! player/*stats* flee-damage)))
         (move-between-refs player/*name*
                            (:inhabitants current-room)
                            (:inhabitants target))
         (ref-set player/*current-room* target)
         (if (seq mobs-here)
           (str "You flee and lose " (max 1 (quot (:max-hp @player/*stats*) 10)) " HP!\n\n" (look))
           (look)))
       "You can't go that way."))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
         (str "You picked up " thing "."))
     (str "There is no " thing " here."))))

(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
         (str "You dropped " thing "."))
     (str "You don't have " thing "."))))

(defn inventory
  "See what you've got."
  []
  (let [inv (seq @player/*inventory*)]
    (if inv
      (str "You are carrying:\n" (str/join "\n" (map name inv)))
      "Your inventory is empty.")))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (player/carrying? "detector")
    (let [item-key (keyword item)]
      (if-let [room (first (filter #(rooms/room-contains? @(second %) item)
                                   @rooms/rooms))]
        (str item " is in " (name (first room)))
        (str item " is not here")))
    "You don't have a detector!"))

(defn use-item
  "Use an item in your inventory."
  [item]
  (if-not (player/carrying? item)
    (str "You don't have " item ".")
    (let [item-key (keyword item)
          potion (items/get-potion item-key)]
      (cond
        ;; Weapon upgrade
        (= item-key :weapon-upgrade)
        (do
          (dosync (alter player/*inventory* disj item-key))
          (player/upgrade-weapon! player/*stats*)
          (str "You upgraded your weapon! Damage increased by 2.\nYour current damage: " (:damage @player/*stats*)))
        
        ;; Potion
        potion
        (do
          (dosync (alter player/*inventory* disj item-key))
          (if-let [heal (:heal potion)]
            (do
              (player/heal! player/*stats* heal)
              (str "You drank " (:name potion) " and restored " heal " HP!\n"
                   "Your HP: " (:hp @player/*stats*) "/" (:max-hp @player/*stats*)))
            (if-let [res (:resist potion)]
              (let [turns (:turns potion 3)]
                (player/apply-resist! player/*stats* res turns)
                (str "You drank " (:name potion) " and gained " res "% resistance for " turns " turns!"))
              (str "You used " (:name potion) ", but nothing happened."))))
        
        :else
        (str "You can't use " item ".")))))

(defn equip-item
  "Equip a weapon or armor from inventory."
  [item-name]
  (if-not (player/carrying? item-name)
    (str "You don't have " item-name " in inventory.")
    (let [weapon (items/get-weapon item-name)
          armor (items/get-armor item-name)]
      (cond
        weapon
        (do
          (dosync (alter player/*inventory* disj (keyword item-name)))
          (player/equip-weapon! player/*stats* weapon)
          (str "You equipped weapon: " (:name weapon) "."))
        
        armor
        (do
          (dosync (alter player/*inventory* disj (keyword item-name)))
          (player/equip-armor! player/*stats* armor)
          (str "You equipped armor: " (:name armor) " (resistance: " (:resist armor) "%)."))
        
        :else
        "This can't be equipped."))))

(defn unequip
  "Remove your weapon."
  []
  (player/equip-weapon! player/*stats* nil)
  "You are now unarmed.")

(defn display-stats
  "Show player stats."
  []
  (let [s @player/*stats*
        xp-needed (player/xp-for-level (:level s))
        weapon (get-in s [:slots :weapon])
        armor (get-in s [:slots :armor])]
    (str "=== Stats ===\n"
         "HP: " (:hp s) "/" (:max-hp s) "\n"
         "Damage: " (:damage s) " (base: " (:base-damage s) ")\n"
         "Weapon: " (if weapon (:name weapon) "Fists") "\n"
         "Armor: " (if armor (str (:name armor) " (" (:resist armor) "%)") "None") "\n"
         "XP: " (:xp s) "/" xp-needed "\n"
         "Level: " (:level s) "\n"
         "Gold: " (:gold s)
         (when (:pending-levelup s)
           "\n\n*** LEVEL UP! Use 'levelup' command ***"))))

(defn levelup-choice
  "Apply level-up bonus based on choice."
  [choice]
  (let [s @player/*stats*]
    (if-not (:pending-levelup s)
      "You have no available level-ups."
      (case choice
        "1" (do (player/apply-level-up! player/*stats* :damage)
                (str "You chose +2 damage!\nYour new base damage: " (:base-damage @player/*stats*)))
        "2" (do (player/apply-level-up! player/*stats* :hp)
                (str "You chose +15 max HP!\nYour new HP: " (:hp @player/*stats*) "/" (:max-hp @player/*stats*)))
        (str "Choose upgrade:\n1) +2 damage\n2) +15 max HP\nUse: levelup 1 or levelup 2")))))

(defn display-help
  "Get help."
  []
  "Explore the dungeon and have fun!")

(defn say
  "Broadcast a message to the current room."
  [& words]
  (let [msg (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*) player/*name*)]
      (if-let [output (get @player/streams inhabitant)]
        (binding [*out* output]
          (println (str player/*name* " says: " msg))
          (flush))))
    (str "You say: " msg)))

(defn attack-mob
  "Attack an enemy in the current room. All mobs attack back."
  [& args]
  (let [room @player/*current-room*
        mob-seq (seq @(:mobs room))]
    (if-not (seq mob-seq)
      "There are no enemies here."
      (let [mob (if (empty? args)
                  (first mob-seq)
                  (mobs/find-mob-in-room room (first args)))]
        (if mob
          (do
            (mobs/player-attack-mob! player/*stats* mob)
            (let [m @mob]
              (if (<= (:hp m) 0)
                (let [gold-reward (or (:gold m) 10)
                      xp-reward (or (:xp m) 10)
                      ;; Drop potion: 65% hp-small, 35% hp-medium
                      potion-drop (if (< (rand) 0.65) :hp-small :hp-medium)
                      potion-name (if (= potion-drop :hp-small) "Small HP Potion" "Medium HP Potion")]
                  (player/add-gold! player/*stats* gold-reward)
                  (player/add-xp! player/*stats* xp-reward)
                  (dosync (alter player/*inventory* conj potion-drop))
                  (mobs/remove-mob-from-room! mob room)
                  ;; Remaining mobs still attack
                  (let [remaining-mobs (seq @(:mobs room))
                        counter-attacks (if remaining-mobs
                                          (str "\n" (mobs/all-mobs-attack! room player/*stats*))
                                          "")]
                    (if (player/alive? player/*stats*)
                      (str "You defeated " (:name m) "! Got " gold-reward " gold and " xp-reward " XP!\n"
                           "Dropped: " potion-name "!" counter-attacks
                           "\nYour HP: " (:hp @player/*stats*) "/" (:max-hp @player/*stats*))
                      (str "You defeated " (:name m) "!" counter-attacks "\n\n*** YOU DIED ***"))))
                ;; All mobs attack back
                (let [attack-result (str "You hit " (:name m) "! [HP: " (:hp m) "/" (:max-hp m) "]\n"
                                         "All enemies attack!\n" (mobs/all-mobs-attack! room player/*stats*))]
                  (if (player/alive? player/*stats*)
                    (str attack-result "\nYour HP: " (:hp @player/*stats*) "/" (:max-hp @player/*stats*))
                    (str attack-result "\n\n*** YOU DIED ***"))))))
          "That mob is not here!")))))

(defn solve-puzzle
  "Show or attempt to solve a puzzle in the current room."
  [& args]
  (let [room @player/*current-room*
        puzzle-ref (:puzzle room)
        puzzle (when puzzle-ref @puzzle-ref)]
    (if-not puzzle
      "There is no puzzle here."
      (if (empty? args)
        (str "Puzzle: \"" (:q puzzle) "\"\n"
             "Choices:\n"
             (str/join "\n" (map-indexed (fn [i c] (str i ") " c)) (:choices puzzle)))
             "\nUse: solve <number>")
        (let [choice-idx (try (Integer/parseInt (first args)) (catch Exception _ nil))]
          (if (nil? choice-idx)
            "Enter a number (0, 1, or 2) to answer."
            (if (<= choice-idx (dec (count (:choices puzzle))))
              (do
                ;; Remove puzzle after any attempt
                (dosync (ref-set puzzle-ref nil))
                (if (= choice-idx (:answer puzzle))
                  (let [gold-reward (+ 20 (rand-int 31))
                        xp-reward (:xp puzzle)]
                    (player/add-gold! player/*stats* gold-reward)
                    (player/add-xp! player/*stats* xp-reward)
                    (str "Correct! You solved the puzzle! Got " xp-reward " XP and " gold-reward " gold!"))
                  (str "Wrong. The correct answer was: " ((:choices puzzle) (:answer puzzle)))))
              "Invalid choice number.")))))))

;; Trader shop items with prices
(def shop-items
  [{:id :hp-small :name "Small HP Potion" :price 20}
   {:id :hp-medium :name "Medium HP Potion" :price 50}
   {:id :resist :name "Resistance Potion" :price 40}
   {:id :sword :name "Sword" :price 80}
   {:id :axe :name "Axe" :price 100}
   {:id :leather :name "Leather Armor" :price 60}
   {:id :chain :name "Chainmail" :price 150}
   {:id :weapon-upgrade :name "Weapon Upgrade (+2 damage)" :price 75}])

(defn trade
  "Trade with a merchant in the current room."
  [& args]
  (let [room @player/*current-room*
        traders (seq @(:traders room))]
    (if-not traders
      "There is no trader here."
      (if (empty? args)
        (str "Welcome to the shop! Your gold: " (player/get-gold player/*stats*) "\n"
             "Items:\n"
             (str/join "\n" (map-indexed (fn [i item] 
                                           (str i ") " (:name item) " - " (:price item) " gold")) 
                                         shop-items))
             "\nUse: trade <number>")
        (let [choice-idx (try (Integer/parseInt (first args)) (catch Exception _ nil))]
          (if (nil? choice-idx)
            "Enter item number."
            (if (and (>= choice-idx 0) (< choice-idx (count shop-items)))
              (let [item (nth shop-items choice-idx)
                    price (:price item)]
                (if (player/spend-gold! player/*stats* price)
                  (do
                    (dosync (alter player/*inventory* conj (:id item)))
                    (str "You bought " (:name item) " for " price " gold!"))
                  (str "Not enough gold! Need " price ", you have " (player/get-gold player/*stats*) ".")))
              "Invalid item number.")))))))

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
