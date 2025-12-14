(ns mire.player)

(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)
(def ^:dynamic *stats*)

(def prompt "> ")
(def streams (ref {}))

(defn carrying? [thing]
  (some #{(keyword thing)} @*inventory*))

(defn init-stats
  "Create a new stats ref for a player with base hp and damage. Returns a ref." 
  [hp damage]
  (ref {:hp hp
        :max-hp hp
        :damage damage
        :base-damage damage  ;; Base damage without weapon
        :xp 0
        :slots {:weapon nil :armor nil :potions #{}}
        :resist_pct 0
        :resist_turns 0}))

(defn heal! [stats-ref amt]
  (dosync
   (alter stats-ref (fn [s]
                      (let [new-hp (min (:max-hp s) (+ (:hp s) amt))]
                        (assoc s :hp new-hp))))))

(defn damage! [stats-ref amt]
  (dosync
   (alter stats-ref (fn [s]
                      (let [; base armor resist from equipped armor
                            armor (:armor (:slots s))
                            armor-resist (if armor (or (:resist armor) 0) 0)
                            resist-pct (or (:resist_pct s) 0)
                            total-resist (min 90 (+ armor-resist resist-pct))
                            effective-dmg (int (Math/round (* amt (/ (- 100 total-resist) 100.0))))
                            new-hp (max 0 (- (:hp s) effective-dmg))
                            ; decrement resist turns if present
                            new-resist-turns (if (and (pos? (:resist_turns s)) (pos? resist-pct))
                                               (dec (:resist_turns s))
                                               (:resist_turns s))
                            new-resist-pct (if (<= (or new-resist-turns 0) 0) 0 resist-pct)]
                        (-> s
                            (assoc :hp new-hp)
                            (assoc :resist_turns new-resist-turns)
                            (assoc :resist_pct new-resist-pct)))))))

(defn alive? [stats-ref]
  (> (:hp @stats-ref) 0))

(defn add-xp! [stats-ref amt]
  (dosync
   (alter stats-ref update :xp + amt)))

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
       ;; No weapon equipped, upgrade base damage (fists)
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
