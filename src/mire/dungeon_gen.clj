(ns mire.dungeon-gen)

(def descriptions
  ["A rough-hewn chamber with dripping water."
   "A narrow tunnel with claw marks on the walls."
   "A vaulted hall filled with echoing footsteps."
   "A cramped storage room cluttered with crates."
   "A quiet grotto lit by phosphorescent moss."])

(def items [:torch :coin :potion :map :scroll])

(defn- random-items []
  (->> items
       (filter (fn [_] (< (rand) 0.25)))
       (into #{})))

(defn- room-id [x y]
  (keyword (str "room-" x "-" y)))

(defn- make-room [x y width height]
  (let [id (room-id x y)
        exits (ref {})]
    (when (> y 0) (dosync (alter exits assoc :north (room-id x (dec y)))))
    (when (< y (dec height)) (dosync (alter exits assoc :south (room-id x (inc y)))))
    (when (> x 0) (dosync (alter exits assoc :west (room-id (dec x) y))))
    (when (< x (dec width)) (dosync (alter exits assoc :east (room-id (inc x) y))))
    {:name id
     :desc (rand-nth descriptions)
     :exits exits
     :items (ref (random-items))
     :inhabitants (ref #{})}))

(defn generate-dungeon
  ([] (generate-dungeon 3 3))
  ([width height]
   (reduce (fn [rooms [x y]]
             (assoc rooms (room-id x y) (make-room x y width height)))
           {}
           (for [x (range width)
                 y (range height)]
             [x y]))))
