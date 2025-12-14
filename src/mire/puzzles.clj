(ns mire.puzzles)

;; Simple puzzle set. Each puzzle has :q question, :choices vector, :answer index, :xp reward

(def puzzles
  [;; Classic riddles
   {:id :p1 :q "What walks on four legs in the morning, two at noon, and three in the evening?"
    :choices ["Cat" "Human" "Table"] :answer 1 :xp 30}
   {:id :p2 :q "I speak without a mouth and hear without ears. What am I?"
    :choices ["Echo" "Wind" "Tree"] :answer 0 :xp 25}
   {:id :p3 :q "What has keys but no locks?"
    :choices ["Piano" "Map" "River"] :answer 0 :xp 20}
   {:id :p4 :q "The more you take, the bigger it becomes. What is it?"
    :choices ["Money" "Hole" "Food"] :answer 1 :xp 25}
   {:id :p5 :q "What can be broken without touching it?"
    :choices ["Glass" "Promise" "Ice"] :answer 1 :xp 30}
   {:id :p6 :q "What has a head and a tail but no body?"
    :choices ["Snake" "Coin" "Comet"] :answer 1 :xp 20}
   {:id :p7 :q "What gets wetter as it dries?"
    :choices ["Sun" "Towel" "Wind"] :answer 1 :xp 25}
   {:id :p8 :q "What belongs to you but others use it more?"
    :choices ["Phone" "Name" "Money"] :answer 1 :xp 30}
   ;; Gaming references
   {:id :p9 :q "How many lives does a cat have?"
    :choices ["7" "9" "1"] :answer 1 :xp 15}
   {:id :p10 :q "What is the strongest block in Minecraft?"
    :choices ["Diamond" "Obsidian" "Bedrock"] :answer 2 :xp 20}
   {:id :p11 :q "Who is the main villain in the Zelda series?"
    :choices ["Bowser" "Ganondorf" "Sephiroth"] :answer 1 :xp 25}
   {:id :p12 :q "Which game is known for the phrase 'The cake is a lie'?"
    :choices ["Half-Life" "Portal" "BioShock"] :answer 1 :xp 30}
   {:id :p13 :q "How many Pokemon were in the first generation?"
    :choices ["150" "151" "152"] :answer 1 :xp 20}
   ;; Math & Logic
   {:id :p14 :q "If 2+2=4, then what is 2×2?"
    :choices ["2" "4" "8"] :answer 1 :xp 15}
   {:id :p15 :q "Continue the sequence: 1, 1, 2, 3, 5, ?"
    :choices ["7" "8" "6"] :answer 1 :xp 25}
   {:id :p16 :q "How many faces does a dodecahedron have?"
    :choices ["10" "12" "20"] :answer 1 :xp 30}
   ;; General knowledge
   {:id :p17 :q "Which planet is closest to the Sun?"
    :choices ["Venus" "Mercury" "Mars"] :answer 1 :xp 20}
   {:id :p18 :q "How many colors are in a rainbow?"
    :choices ["6" "7" "8"] :answer 1 :xp 15}
   {:id :p19 :q "Which chemical element is designated 'Au'?"
    :choices ["Silver" "Gold" "Iron"] :answer 1 :xp 25}
   {:id :p20 :q "Who wrote 'War and Peace'?"
    :choices ["Dostoevsky" "Tolstoy" "Chekhov"] :answer 1 :xp 20}
   ;; Tricky riddles
   {:id :p21 :q "What can you see with your eyes closed?"
    :choices ["Nothing" "Dream" "Darkness"] :answer 1 :xp 30}
   {:id :p22 :q "What has no beginning, end, or middle?"
    :choices ["Circle" "Line" "Point"] :answer 0 :xp 25}
   {:id :p23 :q "What always goes but never leaves?"
    :choices ["River" "Time" "Road"] :answer 1 :xp 30}])

(defn random-puzzle []
  (let [p (rand-nth puzzles)]
    (dissoc p :id)))
