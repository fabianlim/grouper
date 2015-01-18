(ns grouper.number-groups-test
  (:require [clojure.test :refer :all]
            [grouper.number-groups :refer :all]
            [grouper.core-group-functions :as co]  
            ))

;; while integers residue N can be thought of as
;; - integers, or
;; - residue classes
;; we focus on the function aspect rather than the objects
;; hence we simply care if the action that is implemented
;; can apply to whatever objects we supply
(testing "integers residue prime N"
  (let [N 5
        [plus plus-inverse] (action-builder-additive-action-over-residue-integers N)
        [mult mult-inverse] (action-builder-multiplicative-action-over-residue-integers N)
        order (co/build-order-function plus) 
        ]
    (is (= (plus 3 2)0))

    (is (= (plus-inverse 2) 3))
    (is (= (order 2) 5))
    (println (order 3))

    (is (= (mult 3 2) 1))
    (is (= (mult-inverse 2) 3))
    ))

(testing "integers residue nonprime N"
  (let [N 4
        [_ mult-inverse] (action-builder-multiplicative-action-over-residue-integers N)
        ]
    (is (= (mult-inverse 2) nil))
    (is (= (mult-inverse 3) 3))
    ))

(testing "product action"
  (let [[act1 _] (action-builder-additive-action-over-residue-integers 5)
        [act2 _] (action-builder-additive-action-over-residue-integers 4)
        prod-act (co/build-product-action [act1 act2])]
    (prod-act [2 3] [2 1])))
