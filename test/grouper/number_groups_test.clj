(ns grouper.number-groups-test
  (:require [clojure.test :refer :all]
            [grouper.number-groups :refer :all]
            [grouper.core-math-structures :as co]))

(defn elem [x] (co/element (->ZmodN 5) x))
;; testing ZmodN

(testing "ZmodN" (let [N 5
        elem (fn [x] (co/element (->ZmodN N) x))]
    (is (= (co/compose 
          (elem 3) 
          (elem 5)) 
          (elem 3)))

    (is (= (co/identity-element (->ZmodN N)) 
           (elem 0)))
    
    (is (= (co/inverse (elem 2)) 
           (elem 3)))))

; reify
; bad - not top level form
; bad - how to cascade reify

; protocol
; 

; record
; seems I just need a MorphismFromAction record that accepts an action
