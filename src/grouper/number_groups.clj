(ns grouper.number-groups
  (:use [grouper.number-theory :only (extended-euclidean)])
  )

;; TODO ; is there a simple way to test for identity?
;; the name does not imply the object "residues-ints" are impt, 
;; but rather that we used modulo action
(defn action-builder-additive-action-over-residue-integers
  [N]
  " build the actions involved with the plus action on 
    integers mod N "
  [(fn [this other]
     " plus action "
    (clojure.core/mod 
      (clojure.core/+ this other) N))
   (fn [this]
     " inverse "
     (clojure.core/mod (- this) N))
     ])

(defn action-builder-multiplicative-action-over-residue-integers
  [N]
  " build the actions involved with the mult action on 
    integers mod N "
  [(fn [this other]
     " mult action "
    (clojure.core/mod 
      (clojure.core/* this other) N))
   (fn [this]
     " inverse "
     (let [[g u _] (extended-euclidean this N)]
       (if (= g 1)
         (clojure.core/mod u N)
         nil ; return nil if not unit
         )))])
