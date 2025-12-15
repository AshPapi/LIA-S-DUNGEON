(ns mire.player)

(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)
(def ^:dynamic *stats*)

(def prompt "> ")
(def streams (ref {}))

;; Inventory helpers ---------------------------------------------------------

(defn- inc-inventory-count
  "Pure helper to bump an item count in an inventory map."
  [inv item-key]
  (update inv item-key (fnil inc 0)))

(defn- dec-inventory-count
  "Pure helper to decrease an item count in an inventory map, removing the key when it hits zero."
  [inv item-key]
  (let [cur (get inv item-key 0)]
    (cond
      (> cur 1) (assoc inv item-key (dec cur))
      (= cur 1) (dissoc inv item-key)
      :else inv)))

(defn add-item!
  "Add one instance of item-key to the bound inventory ref."
  [item-key]
  (dosync
   (alter *inventory* inc-inventory-count (keyword item-key))))

(defn remove-item!
  "Remove one instance of item-key from the bound inventory ref. Returns true if something was removed."
  [item-key]
  (let [k (keyword item-key)]
    (dosync
     (let [before (get @*inventory* k 0)]
       (when (pos? before)
         (alter *inventory* dec-inventory-count k)
         true)))))

(defn inventory-items
  "Return a seq of item keywords, repeating entries based on their counts."
  []
  (mapcat (fn [[k c]] (repeat c k)) @*inventory*))

(defn inventory-unique
  "Return the set of unique items currently held."
  []
  (keys @*inventory*))

;; Stats and combat ----------------------------------------------------------

(defn xp-for-level [level]
  (* 50 level))

(defn carrying? [thing]
  (pos? (get @*inventory* (keyword thing) 0)))

(defn init-stats
  "Create a new stats ref for a player with base hp and damage. Returns a ref."
  [hp damage]
  (ref {:hp hp
        :max-hp hp
        :damage damage
        :base-damage damage
        :xp 0
        :level 1
        :pending-levelup false
        :gold 0
        :slots {:weapon nil :armor nil :potions #{}}
        :resist_pct 0
        :resist_turns 0}))

(defn heal! [stats-ref amt]
  (dosync
   (alter stats-ref (fn [s]
                      (let [new-hp (min (:max-hp s) (+ (:hp s) amt))]
                        (assoc s :hp new-hp))))))

(defn damage!
  [stats-ref amt]
  (dosync
   (alter stats-ref (fn [s]
                      (let [armor (:armor (:slots s))
                            armor-resist (if armor (or (:resist armor) 0) 0)
                            buff-resist (or (:resist_pct s) 0)
                            total-resist (min 90 (+ armor-resist buff-resist))
                            effective-dmg (int (Math/round (* amt (/ (- 100 total-resist) 100.0))))
                            new-hp (max 0 (- (:hp s) effective-dmg))
                            new-resist-turns (if (and (pos? (:resist_turns s)) (pos? buff-resist))
                                               (dec (:resist_turns s))
                                               (:resist_turns s))
                            new-resist-pct (if (<= (or new-resist-turns 0) 0) 0 buff-resist)]
                        (-> s
                            (assoc :hp new-hp)
                            (assoc :resist_turns new-resist-turns)
                            (assoc :resist_pct new-resist-pct)))))))

(defn alive? [stats-ref]
  (> (:hp @stats-ref) 0))

(defn check-level-up [stats-ref]
  "Check if player has enough XP to level up. Returns :levelup if pending, nil otherwise."
  (let [s @stats-ref
        threshold (xp-for-level (:level s))]
    (when (and (>= (:xp s) threshold) (not (:pending-levelup s)))
      (dosync
       (alter stats-ref assoc :pending-levelup true))
      :levelup)))

(defn apply-level-up! [stats-ref choice]
  "Apply level-up bonus based on choice: :damage or :hp"
  (dosync
   (alter stats-ref (fn [s]
                      (let [threshold (xp-for-level (:level s))
                            new-xp (- (:xp s) threshold)
                            base-update (-> s
                                            (assoc :xp new-xp)
                                            (update :level inc)
                                            (assoc :pending-levelup false))]
                        (case choice
                          :damage (update base-update :base-damage + 2)
                          :hp (-> base-update
                                  (update :max-hp + 15)
                                  (update :hp + 15))
                          base-update))))))

(defn add-xp! [stats-ref amt]
  (dosync
   (alter stats-ref update :xp + amt))
  (check-level-up stats-ref))

(defn equip-weapon! [stats-ref weapon]
  (dosync
   (alter stats-ref assoc-in [:slots :weapon] weapon)
   (alter stats-ref (fn [s]
                      (let [base (:base-damage s)
                            weapon-dmg (or (:damage weapon) 0)]
                        (assoc s :damage (+ base weapon-dmg)))))))

(defn upgrade-weapon! [stats-ref]
  "Upgrade equipped weapon damage by 2. Returns true if successful."
  (dosync
   (let [s @stats-ref
         weapon (get-in s [:slots :weapon])]
     (if weapon
       (do
         (alter stats-ref update-in [:slots :weapon :damage] + 2)
         (alter stats-ref update :damage + 2)
         true)
       (do
         (alter stats-ref update :base-damage + 2)
         (alter stats-ref update :damage + 2)
         true)))))

(defn equip-armor! [stats-ref armor]
  (dosync
   (alter stats-ref assoc-in [:slots :armor] armor)))

(defn apply-resist! [stats-ref pct turns]
  "Apply a percentage damage resistance for a number of turns to stats-ref."
  (dosync
   (alter stats-ref assoc :resist_pct pct)
   (alter stats-ref assoc :resist_turns turns)))

(defn add-gold! [stats-ref amt]
  "Add gold to the player's stats."
  (dosync
   (alter stats-ref update :gold + amt)))

(defn spend-gold! [stats-ref amt]
  "Spend gold from the player's stats. Returns true if successful."
  (dosync
   (if (>= (:gold @stats-ref) amt)
     (do (alter stats-ref update :gold - amt)
         true)
     false)))

(defn get-gold [stats-ref]
  "Get the player's current gold amount."
  (:gold @stats-ref))
