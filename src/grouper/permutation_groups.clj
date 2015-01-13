(ns grouper.permutation-groups
  (:require [grouper.core-objects :as co])) 

(def )

(defn gcd [x y]
  (cond
    (> x y) (gcd (- x y) y)
    (< x y) (gcd x (- y x))
  :else x))

(gcd 21 6)

(::key)
