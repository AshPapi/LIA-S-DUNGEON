(ns mire.items)

(def weapons
  {:fists {:name "Fists" :damage 4 :min-damage 2 :max-damage 6}
   :dagger {:name "Dagger" :damage 5 :min-damage 3 :max-damage 7}
   :club {:name "Club" :damage 4 :min-damage 2 :max-damage 6}
   :sword {:name "Sword" :damage 7 :min-damage 4 :max-damage 9}
   :axe {:name "Axe" :damage 8 :min-damage 5 :max-damage 10}
   :spear {:name "Spear" :damage 9 :min-damage 6 :max-damage 11}
   :mace {:name "Mace" :damage 10 :min-damage 7 :max-damage 12}
   :warhammer {:name "Warhammer" :damage 12 :min-damage 8 :max-damage 14}
   :crossbow {:name "Crossbow" :damage 11 :min-damage 7 :max-damage 13}})

(def armors
  {:none {:name "Clothes" :resist 0}
   :leather {:name "Leather Armor" :resist 10}
   :scale {:name "Scale Armor" :resist 18}
   :chain {:name "Chainmail" :resist 25}
   :plate {:name "Plate Armor" :resist 35}})

(def potions
  {:hp-small {:name "Small HP Potion" :heal 20}
   :hp-medium {:name "Medium HP Potion" :heal 50}
   :resist {:name "Resistance Potion" :resist 25 :turns 3}})

(def upgrade-kit
  {:name "Weapon Upgrade" :damage-bonus 2})

(defn get-weapon [k]
  (when k (get weapons (keyword k))))

(defn get-armor [k]
  (when k (get armors (keyword k))))

(defn get-potion [k]
  (when k (get potions (keyword k))))

(defn is-upgrade? [k]
  (= (keyword k) :weapon-upgrade))

(defn weapon-damage [weapon]
  (or (:damage weapon) 0))

(defn armor-resist [armor]
  (or (:resist armor) 0))

(defn use-potion! [stats-ref potion]
  "Apply a potion effect to a player's stats ref. Currently only HP and resistance potions are supported."
  (when-let [p (get-potion potion)]
    (require 'mire.player)
    (if-let [heal (:heal p)]
      (do
        ((resolve 'mire.player/heal!) stats-ref heal)
        (str "You drank " (:name p) " and restored " heal " HP."))
      (if-let [res (:resist p)]
        (let [turns (:turns p 3)]
          ((resolve 'mire.player/apply-resist!) stats-ref res turns)
          (str "You drank " (:name p) " and gained " res "% resistance for " turns " turns."))
        (str "You used " (:name p) ", but nothing happened.")))))
