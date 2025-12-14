(ns mire.player)

(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)

(def prompt "> ")
(def streams (ref {}))

(defn carrying?
  "Return true if the current player is carrying the given item keyword or name."
  [thing]
  (some #{(keyword thing)} @*inventory*))
