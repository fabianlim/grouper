(ns grouper.free-groups-test
  (:require [clojure.test :refer :all]
            [grouper.free-groups :refer :all]))


(testing "simplify relations" 
  ;; this is the worst case, my implementation is slow in this case
  (is (= (simplify-relations "CAbBac") "")) 
  (is (= (simplify-relations "BbaAaBB") "aBB")))

(testing "inversions" 
  ;; doesnt simplify 
  (is (= (inverse-word "CAbBac") "CAbBac")) 
  )

(testing "following edges in Scherier graphs" 

  (is (= (follow-edge-labels-forward {[1 "a"] 2 [1 "B"] 3} 1 "ab") [2 "b"]))
  (is (= (follow-edge-labels-backward {[1 "a"] 2 [1 "B"] 3} 1 "ab") [3 "a"]))

  (let [graph
        {[1 "B"] 2,
         [1 "A"] 1,
         [1 "a"] 1,
         [1 "b"] nil,
         [2 "B"] nil,
         [2 "b"] 1,
         [2 "A"] nil,
         [2 "a"] nil}]

    (is (= (follow-edge-labels-forward graph 2 "bbb") [1 "bb"]))
    (is (= (scan-relation graph {:rel "bbb" :nodes [2 2]}) {:rel "bb" :nodes [1 2]}))
    ))

(testing "Todd-Coxeter"
  ;; only take last if you are *ABSOLUTELY* sure that Todd-Coxeter will converge
  ;; in a reasonable amount of time
  ;; this presentation does not require coincidences
  (is (= (:graph (last (Todd-Coxeter-procedure ["aaa" "bbb" "abab"] ["a"])))
           {[4 "A"] 3,
            [2 "b"] 3,
            [3 "B"] 2,
            [2 "B"] 1,
            [3 "a"] 4,
            [4 "B"] 4,
            [4 "a"] 2,
            [2 "a"] 3,
            [1 "B"] 3,
            [4 "b"] 4,
            [1 "A"] 1,
            [2 "A"] 4,
            [3 "A"] 2,
            [1 "a"] 1,
            [1 "b"] 2,
            [3 "b"] 1}))
  ; (pp/pprint (Todd-Coxeter-procedure ["aaa" "bbb" "abab"] ["a"]))
  )


;   [2 "b"] 3,
;   [3 "a"] 4,
;   [4 "a"] 2,
;   [2 "a"] 3,
;   [4 "b"] 4,
;   [1 "a"] 1,
;   [1 "b"] 2,
;   [3 "b"] 1},
