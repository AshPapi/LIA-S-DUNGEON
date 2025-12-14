(ns mire.rooms
  (:require [mire.dungeon-gen :as dungeon-gen]))

(def rooms (ref {}))

(defn- start-room [dungeon]
  (let [north (when (contains? dungeon :room-0-1) :room-0-1)
        east (when (contains? dungeon :room-1-0) :room-1-0)
        exits (-> {}
                  (cond-> north (assoc :north north))
                  (cond-> east (assoc :east east)))]
    {:name :start
     :desc "You stand at the mouth of a newly carved dungeon."
     :exits (ref exits)
     :items (ref #{})
     :inhabitants (ref #{})}))

(defn add-rooms
  ([] (add-rooms nil true))
  ([dir] (add-rooms dir true))
  ([dir use-procedural?]
   (if use-procedural?
     (let [dungeon (dungeon-gen/generate-dungeon)
           start (start-room dungeon)]
       (dosync
        (ref-set rooms (assoc dungeon :start start))))
     (dosync
      (ref-set rooms {})))))

(defn current [room-name]
  (@rooms room-name))

(defn room-contains? [room thing]
  (@(:items room) (keyword thing)))
