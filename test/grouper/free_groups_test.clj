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

  ;; coincidence processing
  (let [graph
         {[2 "b"] 3,
          [3 "B"] 2,
          [4 "B"] 5,
          [4 "a"] 3,
          [2 "a"] 6,
          [5 "B"] 1,
          [1 "B"] 6,
          [6 "A"] 2,
          [5 "b"] 4,
          [1 "A"] 5,
          [2 "A"] 1,
          [6 "b"] 1,
          [3 "A"] 4,
          [1 "a"] 2,
          [1 "b"] 5,
          [5 "a"] 1}
          coset-meta {:coset-next-label 7 
                      :equivalences {6 2} 
                      :process-queue (conj clojure.lang.PersistentQueue/EMPTY 6)}]
        (is (= (nth (iterate Todd-Coxeter-process-coincidence 
                                 {:graph graph :coset-meta coset-meta}) 5)
                {:graph {[1 "B"] 1, [1 "A"] 1, [1 "a"] 1, [1 "b"] 1},
                 :coset-meta
                 {:coset-next-label 7,
                  :equivalences {4 1, 5 1, 3 1, 2 1, 6 2},
                  :process-queue clojure.lang.PersistentQueue/EMPTY}
                  :comment "process 4"
                 })))

  ;; trival group
  (is (= (:graph (last (Todd-Coxeter-procedure ["abABB" "baBAA"] [])))
           {[1 "A"] 1,
            [1 "a"] 1,
            [1 "B"] 1,
            [1 "b"] 1}))

  ;; start from some initial point
  (let [generators ["a" "b"]
        relations ["abABB" "baBAA"]
        machine (Todd-Coxeter-builder-state-machine generators relations [])
        edge (fn [from e to] {[from e] to [to (inverse-word e)] from})
        graph (reduce into  ;; initialize with these edges
                  (vector (edge 1 "a" 2) 
                          (edge 2 "b" 3) 
                          (edge 3 "A" 4)
                          (edge 4 "B" 5))) 
        state (-> (Todd-Coxeter-builder-initial-state generators relations)
                 (assoc-in [:coset-meta :coset-next-label] 6)
                 (assoc :graph graph)
                 (assoc-in [:r-queues :unscanned] 
                           (vector 
                             {:rel "B" :nodes [5 1] :row 5} {:rel "baBAA" :nodes [5 5] :row 5}
                             {:rel "baBAA" :nodes [1 1] :row 1} {:rel "abABB" :nodes [1 1] :row 1}
                             {:rel "baBAA" :nodes [2 2] :row 2} {:rel "abABB" :nodes [2 2] :row 2}
                             {:rel "baBAA" :nodes [3 3] :row 3} {:rel "abABB" :nodes [3 3] :row 3}
                             {:rel "baBAA" :nodes [4 4] :row 4} {:rel "abABB" :nodes [4 4] :row 4}
                             )))
        x (take-while (comp not nil?) (iterate machine state))]  
    (is (= (:graph (last x))
           {[1 "A"] 1,
            [1 "a"] 1,
            [1 "B"] 1,
            [1 "b"] 1}))

  ))

