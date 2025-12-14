(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]))

(defn- move-between-refs [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn look []
  (let [room @player/*current-room*
        exits (->> @(:exits room) keys (map name) sort (str/join ", "))
        items (seq @(:items room))
        others (disj @(:inhabitants room) player/*name*)]
    (str (:desc room)
         (when (seq exits) (str "\nExits: " exits))
         (when items (str "\nYou see: " (->> items (map name) sort (str/join ", "))))
         (when (seq others)
           (str "\nAlso here: " (->> others sort (str/join ", ")))))))

(defn move [direction]
  (let [dir (keyword (str/lower-case direction))]
    (dosync
     (let [target-name ((:exits @player/*current-room*) dir)
           target (when target-name (rooms/current target-name))]
       (if target
         (do (move-between-refs player/*name*
                                (:inhabitants @player/*current-room*)
                                (:inhabitants target))
             (ref-set player/*current-room* target)
             (look))
         "You can't go that way.")))))

(defn grab [item]
  (if (str/blank? item)
    "Grab what?"
    (dosync
     (let [thing (keyword item)
           room @player/*current-room*]
       (if (rooms/room-contains? room thing)
         (do (move-between-refs thing (:items room) player/*inventory*)
             (str "You pick up " (name thing) "."))
         (str "There is no " item " here."))))))

(defn drop [item]
  (if (str/blank? item)
    "Drop what?"
    (dosync
     (let [thing (keyword item)]
       (if (player/carrying? item)
         (do (move-between-refs thing player/*inventory* (:items @player/*current-room*))
             (str "You drop " (name thing) "."))
         (str "You are not carrying " item "."))))))

(defn inventory []
  (let [items (seq @player/*inventory*)]
    (if items
      (str "You carry: " (->> items (map name) sort (str/join ", ")))
      "Your inventory is empty.")))

(defn help []
  "Commands: look, move <dir>, grab <item>, drop <item>, inventory, help, quit")

(def commands
  {"look" (fn [& _] (look))
   "move" (fn [direction & _]
             (if direction
               (move direction)
               "Move where?"))
   "north" (fn [& _] (move "north"))
   "south" (fn [& _] (move "south"))
   "east" (fn [& _] (move "east"))
   "west" (fn [& _] (move "west"))
   "grab" (fn [item & more]
             (grab (str/join " " (cons (or item "") more))))
   "drop" (fn [item & more]
             (drop (str/join " " (cons (or item "") more))))
   "inventory" (fn [& _] (inventory))
   "help" (fn [& _] (help))})

(defn execute [input]
  (let [[command & args] (str/split input #"\s+")
        action (get commands command)]
    (if action
      (apply action args)
      (str "Unknown command: " command ". Type 'help' for assistance."))))
