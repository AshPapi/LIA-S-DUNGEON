(ns mire.rooms)

(def rooms (ref {}))

(defn- make-room [id desc exits items]
  {:name id
   :desc desc
  :exits (ref exits)
   :items (ref items)
   :inhabitants (ref #{})})

(defn- build-static-rooms []
  (let [start (make-room :start
                         "You are in a small antechamber carved into the rock."
                         {:north :hallway :east :armory}
                         #{:torch})
        hallway (make-room :hallway
                           "A drafty hallway stretches into the darkness."
                           {:south :start :east :library}
                           #{:coin})
        armory (make-room :armory
                          "Rusty weapons hang from the walls of this cramped room."
                          {:west :start}
                          #{:sword})
        library (make-room :library
               "Dusty shelves are packed with crumbling tomes."
               {:west :hallway}
               #{:scroll})]
    {:start start
     :hallway hallway
     :armory armory
     :library library}))

(defn add-rooms
  ([] (add-rooms nil))
  ([_] (dosync (ref-set rooms (build-static-rooms)))))

(defn current [room-name]
  (@rooms room-name))

(defn room-contains? [room thing]
  (@(:items room) (keyword thing)))
