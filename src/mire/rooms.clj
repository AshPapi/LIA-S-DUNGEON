(ns mire.rooms
  (:require [mire.mobs :as mobs]
            [mire.dungeon-gen :as dungeon-gen]))

(def rooms (ref {}))

(defn load-room [rooms file]
  (let [room (read-string (slurp (.getAbsolutePath file)))]
    (conj rooms
          {(keyword (.getName file))
           {:name (keyword (.getName file))
            :desc (:desc room)
            :exits (ref (:exits room))
            :items (ref (or (:items room) #{}))
            :inhabitants (ref #{})
            :mobs (ref #{})}})))

(defn load-rooms
  "Given a dir, return a map with an entry corresponding to each file
  in it. Files should be maps containing room data."
  [rooms dir]
  (dosync
   (reduce load-room rooms
           (.listFiles (java.io.File. dir)))))

(defn add-rooms
  ([dir use-procedural?]
     (if use-procedural?
       (dosync
        (ref-set rooms (assoc (dungeon-gen/generate-full-dungeon) :start
                             {:name :start
                              :desc "You find yourself in the entrance to a vast underground labyrinth."
                              :exits (ref {:north :room-0-1 :east :room-1-0})
                              :items (ref #{})
                              :inhabitants (ref #{})
                              :mobs (ref #{})})))
       (dosync
        (alter rooms load-rooms dir))))
  ([dir]
     (add-rooms dir true)))

(defn room-contains?
  [room thing]
  (@(:items room) (keyword thing)))
