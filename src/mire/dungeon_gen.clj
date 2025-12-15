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
  [[:dagger 0.12] [:club 0.14] [:sword 0.10] [:axe 0.08] [:spear 0.07]
   [:mace 0.06] [:warhammer 0.04] [:crossbow 0.05] [:weapon-upgrade 0.05]
   [:leather 0.12] [:scale 0.10] [:chain 0.07] [:plate 0.04]
   [:hp-small 0.25] [:hp-medium 0.15] [:resist 0.10]])


(def trader-types
  [{:type :general :name "Old Merchant" :desc "An experienced trader with many goods"}
   {:type :scavenger :name "Hermit" :desc "A mysterious hermit from the depths"}
   {:type :blacksmith :name "Blacksmith" :desc "A rough blacksmith with powerful equipment"}
   {:type :alchemist :name "Alchemist" :desc "An alchemist brewing strange potions"}
   {:type :fortune :name "Fortune Teller" :desc "A mystical fortune teller with cryptic words"}])


(defn- weighted-pick
  "Pick one item from pool of [item weight] with probability proportional to weight."
  [pool]
  (let [total (reduce + (map second pool))
        r (* total (rand))]
    (loop [[[item weight] & more] pool
           acc 0]
      (let [acc' (+ acc weight)]
        (cond
          (>= acc' r) item
          (nil? more) item
          :else (recur more acc'))))))


(defn generate-room-items []
  ;; Less frequent loot: ~65% rooms empty, ~25% with one item, ~10% with two.
  (let [roll (rand)]
    (cond
      (< roll 0.65) #{}
      (< roll 0.90) (set [(weighted-pick items-pool)])
      :else (->> (repeatedly #(weighted-pick items-pool))
                 distinct
                 (take 2)
                 set))))



(defn generate-room-content []
  (let [room-type (rand)
        base {:items (ref (generate-room-items))
              :mobs (ref #{})
              :traders (ref [])
              :puzzle (ref nil)
              :inhabitants (ref #{})
              :room-type nil}]
    (cond
      
      (< room-type 0.55)
      (assoc base :room-type :mob)
      
      
      (< room-type 0.95)
      (assoc base :puzzle (ref (puzzles/random-puzzle)))
      
      
      :else
      (assoc base :traders (ref [(rand-nth trader-types)])))))


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


(def dungeon-width 10)
(def dungeon-height 10)

(defn generate-full-dungeon []
  (let [rooms {}]
    
    (reduce (fn [rooms [x y]]
              (let [room-id (keyword (str "room-" x "-" y))
                    exits (ref {})
                    content (generate-room-content)]
                
                
                (when (> y 0) (dosync (alter exits assoc :north (keyword (str "room-" x "-" (dec y))))))
                (when (< y (dec dungeon-height)) (dosync (alter exits assoc :south (keyword (str "room-" x "-" (inc y))))))
                (when (> x 0) (dosync (alter exits assoc :west (keyword (str "room-" (dec x) "-" y)))))
                (when (< x (dec dungeon-width)) (dosync (alter exits assoc :east (keyword (str "room-" (inc x) "-" y)))))
                
                (let [room {:name room-id
                            :desc (rand-nth room-descriptions)
                            :exits exits
                            :items (:items content)
                            :mobs (:mobs content)
                            :traders (:traders content)
                            :puzzle (:puzzle content)
                            :inhabitants (:inhabitants content)
                            :room-type (:room-type content)}]
                  
                  (add-mobs-to-room room)
                  (assoc rooms room-id room))))
            rooms
            (for [x (range dungeon-width) y (range dungeon-height)] [x y]))))
