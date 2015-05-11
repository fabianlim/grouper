;; examples/permutation_groups/rubik_cube
(ns grouper.examples.permutation-groups.rubik-cube
  (:require [grouper.permutation-groups :as pg]
            [grouper.schreier-sims :as ss]
            [grouper.core-group-functions :as co]))

;; face-rotation-builder
(defn face-rotation-builder
  [F U D L R]
  (pg/cyc-notation [ #{F U L} 
                     #{F U R} 
                     #{F D R} 
                     #{F D L} ]
                   [ #{F U} 
                     #{F R}
                     #{F D}
                     #{F L} ]))

;; face 
(def face-rotations 
   {"f" (face-rotation-builder \F \U \D \L \R)
    "r" (face-rotation-builder \R \U \D \F \B) 
    "l" (face-rotation-builder \L \U \D \B \F) 
    "b" (face-rotation-builder \B \U \D \R \L) 
    "u" (face-rotation-builder \U \B \F \L \R) 
    "d" (face-rotation-builder \D \F \B \L \R)})

; (def p-bsgs (ss/partial-BSGS [#{\F \U}] {} face-rotations first))
; 
; (def initial-state (ss/Schreier-Sims-initial-state p-bsgs first))
(pg/cyc-notation [1 2])

   {"f" (pg/cyc-notation [17 19 24 22] [18 21 23 20] 
                    [6 25 43 16] [7 28 42 13] [8 30 41 11])}
(def face-rotations
   {"f" (pg/cyc-notation [17 19 24 22] [18 21 23 20] 
                    [6 25 43 16] [7 28 42 13] [8 30 41 11])
   "b" (pg/cyc-notation [33 35 40 38] [34 37 39 36] 
                    [3 9 46 32] [2 12 47 29] [1 14 48 27])
   "l" (pg/cyc-notation [9 11 16 14] [10 13 15 12] 
                    [1 17 41 40] [4 20 44 37] [6 22 46 35])
   "r" (pg/cyc-notation [25 27 32 30] [26 29 31 28] 
                    [3 38 43 19] [5 36 45 21] [8 33 48 24])
   "u" (pg/cyc-notation [1 3 8 6] [2 5 7 4] [9 33 25 17]
                    [10 34 26 18] [11 35 27 19]
                    [14 22 30 38] [15 23 31 39] [16 24 32 40])
   "d" (pg/cyc-notation [41 43 48 46] [42 45 47 44])})

; (map (comp pg/order second) face-rotations)
(def p-bsgs (ss/partial-BSGS [1 2] {} face-rotations))
(face-rotations "f")
(pg/moved-points-other-than (face-rotations "f") [17])
(def other-points #(pg/moved-points-other-than % [17]))

(apply min (reduce #(into %1 (other-points %2)) #{} [(face-rotations "f")]))
