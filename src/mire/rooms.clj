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
                         "You stand in a dim stone chamber."
                         {:north :hallway :east :armory}
                         #{:torch})
        hallway (make-room :hallway
                           "A narrow hallway echoes with distant dripping."
                           {:south :start :east :library}
                           #{:coin})
        armory (make-room :armory
                          "Stacks of battered shields line the walls."
                          {:west :start}
                          #{:helmet})
        library (make-room :library
                            "Ancient tomes gather dust here."
                            {:west :hallway}
                            #{:scroll})]
    {:start start
     :hallway hallway
     :armory armory
     :library library}))

(defn add-rooms []
  (dosync (ref-set rooms (build-static-rooms))))

(defn current [room-name]
  (@rooms room-name))

(defn room-contains? [room thing]
  (@(:items room) (keyword thing)))
