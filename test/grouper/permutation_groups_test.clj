(ns grouper.permutation-groups-test
  (:require [clojure.test :refer :all]
            [grouper.permutation-groups :refer :all]))

(testing "permutation groups"

  ;; works with numbers
  (is (= (compose {:1 :2, :2 :1} {:1 :3, :3 :1})
         (cyc :1 :2 :3)))

  ;; non-commutative
  (is (= (compose {:1 :3, :3 :1} {:1 :2, :2 :1} )
         (cyc :1 :3 :2)))

  ;; works with letters
  (is (= (compose {:a :b, :b :a} {:a :c, :c :a})
         (cyc :a :b :c)))

  ;; mixture - direct product
  (is (= (compose {:1 :2, :2 :1} {:a :b, :b :a})
         (compose (cyc :1 :2) (cyc :a :b))))

  ;; disjoint cycles are commutative
  (is  (= (compose  (cyc :1 :2 :3) (cyc :a :b))
          (compose  (cyc :a :b) (cyc :1 :2 :3) )))

  ;; first cycle
  (is (= (first-cyc (compose (cyc :c :d) (cyc :a :b))) 
         (cyc :c :d)))

  ;; cycle-notation
  (is (= (cycle-notation [1 2] [3 4])  
        (compose (cyc 1 2) (cyc 3 4))))
  
  ;; cycle-notation will drop disjoint cycles
  (is (= (cycle-notation [1 2] [1 4])  
        (cyc 1 2)))
  )

;; recursive
;; (reduce cyc [(cyc :1 :2) (cyc :2 :3)])
