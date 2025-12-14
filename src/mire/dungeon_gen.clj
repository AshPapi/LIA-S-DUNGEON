(ns mire.dungeon-gen
  (:require [mire.mobs :as mobs]
            [mire.items :as items]
            [mire.puzzles :as puzzles]))
(def room-descriptions
  ["You stand in a dimly lit room with stone walls."
   "A huge underground cavern opens before you."
   "You find yourself in a narrow tunnel carved into rock."
   "This room smells of dampness and decay."
   "Ancient writings cover the walls."
   "A majestic hall with a high ceiling stretches before you."
   "The floor here is slippery from moisture."
   "Piles of stones tell of a recent collapse."
   "Torches flicker on the walls, casting dancing shadows."
   "The musty smell of ancient treasures fills the air."
   "Crystal formations shimmer in the darkness."
   "The walls here bear marks of battle."
   "Mysterious symbols are engraved in the stone."
   "Water drips steadily from the ceiling."
   "The air here is cold and oppressive."])
(def items-pool
  [[:sword 0.10] [:club 0.12] [:axe 0.08]
   [:leather 0.12] [:chain 0.06]
   [:hp-small 0.25] [:hp-medium 0.15] [:resist 0.10]])

(defn generate-room-items []
  (let [items (ref #{})]
    (doseq [[item-key prob] items-pool]
      (when (< (rand) prob)
        (dosync (alter items conj item-key))))
    @items))

(defn generate-room-content []
  (let [room-type (rand)
        base {:items (ref (generate-room-items))
              :mobs (ref #{})
              :puzzle (ref nil)
              :inhabitants (ref #{})
              :room-type nil}]
    (cond
      (< room-type 0.65)
      (assoc base :room-type :mob)
  :else
  (assoc base :puzzle (ref (puzzles/random-puzzle))))))
(defn add-mobs-to-room [room]
  (when (= (:room-type room) :mob)
    (let [mob-chance (rand)
          all-mob-types mobs/mob-types]
      (cond
        (< mob-chance 0.10)
        (do (mobs/add-mob-to-room (rand-nth all-mob-types) room)
            (mobs/add-mob-to-room (rand-nth all-mob-types) room))
        (< mob-chance 0.50)
        (mobs/add-mob-to-room (rand-nth [:zombie :skeleton :ghost]) room)
        (< mob-chance 0.75)
        (mobs/add-mob-to-room (rand-nth [:witch :varpach :pudge]) room)
        (< mob-chance 0.90)
        (mobs/add-mob-to-room (rand-nth [:berezutskie :slenderman]) room)
        :else
        (mobs/add-mob-to-room :herobrine room))))
  room)

(defn generate-full-dungeon []
  (let [rooms {}]
    (reduce (fn [rooms [x y]]
              (let [room-id (keyword (str "room-" x "-" y))
                    exits (ref {})
                    content (generate-room-content)]
                (when (> y 0) (dosync (alter exits assoc :north (keyword (str "room-" x "-" (dec y))))))
                (when (< y 9) (dosync (alter exits assoc :south (keyword (str "room-" x "-" (inc y))))))
                (when (> x 0) (dosync (alter exits assoc :west (keyword (str "room-" (dec x) "-" y)))))
                (when (< x 4) (dosync (alter exits assoc :east (keyword (str "room-" (inc x) "-" y)))))
                
                (let [room {:name room-id
                            :desc (rand-nth room-descriptions)
                            :exits exits
                            :items (:items content)
                            :mobs (:mobs content)
                            :puzzle (:puzzle content)
                            :inhabitants (:inhabitants content)
                            :room-type (:room-type content)}]
                  (add-mobs-to-room room)
                  (assoc rooms room-id room))))
            rooms
            (for [x (range 5) y (range 10)] [x y]))))
