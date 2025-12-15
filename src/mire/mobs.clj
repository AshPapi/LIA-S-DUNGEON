(ns mire.mobs
  (:require [mire.player :as player]
            [clojure.string :as str]))



(def mobs
  {:zombie {:name "Zombie" :hp 25 :max-hp 25 :damage 4 :xp 18 :gold 6}
   :skeleton {:name "Skeleton" :hp 20 :max-hp 20 :damage 5 :xp 23 :gold 9}
   :witch {:name "Witch" :hp 32 :max-hp 32 :damage 6 :xp 35 :gold 18}
   :berezutskie {:name "Twin Brothers" :hp 55 :max-hp 55 :damage 11 :xp 70 :gold 36}
   :pudge {:name "Pudge" :hp 80 :max-hp 80 :damage 8 :xp 80 :gold 42}
   :ghost {:name "Ghost" :hp 16 :max-hp 16 :damage 9 :xp 30 :gold 15}
   :slenderman {:name "Slenderman" :hp 65 :max-hp 65 :damage 12 :xp 105 :gold 60}
   :herobrine {:name "Herobrine" :hp 95 :max-hp 95 :damage 14 :xp 130 :gold 72}
   :varpach {:name "Dark Knight" :hp 48 :max-hp 48 :damage 7 :xp 45 :gold 24}})

(def mob-types (vec (keys mobs)))

(defn spawn
  "Return an atom holding a mob instance cloned from the mob definition."
  [k]
  (when-let [m (get mobs (keyword k))]
    (atom (assoc m :id (keyword k) :hp (:hp m)))))

(defn add-mob-to-room
  "Create a mob instance for key `k` and add it to the room's :mobs ref.
   `room` should be a room map (not a ref). Returns the mob atom."
  [k room]
  (let [mob-atom (spawn k)]
    (when mob-atom
      (dosync
        (alter (:mobs room) conj mob-atom)))
    mob-atom))

(defn remove-mob-from-room!
  "Remove a mob-atom from a room's :mobs ref. Call inside a transaction or uses dosync.
   Returns true if removed."
  [mob-atom room]
  (dosync
   (alter (:mobs room) disj mob-atom)))

(defn find-mob-in-room
  "Find a mob atom in a room by keyword id or nil to return any mob."
  [room id]
  (let [all @(:mobs room)]
    (if id
      (first (filter #(= (:id @%) (keyword id)) all))
      (first all))))

(defn mob-attack!
  "Mob attacks a player stats ref. Uses player/damage! to apply damage."
  [mob-atom player-stats-ref]
  (let [dmg (:damage @mob-atom 0)]
    (player/damage! player-stats-ref dmg)
    (str (:name @mob-atom) " hits you for " dmg " damage.")))

(defn all-mobs-attack!
  "All mobs in the room attack the player. Returns combined message."
  [room player-stats-ref]
  (let [mob-seq (seq @(:mobs room))]
    (if (empty? mob-seq)
      ""
      (str/join "\n" (map #(mob-attack! % player-stats-ref) mob-seq)))))

(defn player-attack-mob!
  "Player (stats-ref) attacks mob-atom. Returns message and applies damage to mob."
  [player-stats mob-atom]
  (let [pdmg (:damage @player-stats)]
    (swap! mob-atom update :hp (fn [h] (max 0 (- h pdmg))))
    (str "You dealt " pdmg " damage to " (:name @mob-atom) ".")))
