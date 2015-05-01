(ns grouper.schreier-sims-test
  (:require [clojure.test :refer :all]
            [grouper.schreier-sims :refer :all]
            [grouper.permutation-groups :as pg]))

(testing "BSGS Sym4"
  (let [Sym4 {:base (vector 1 2 3)
              :sgs (vector 
                    {:lvl 1 :gens {"a" (pg/cyc-notation [1 2])}}
                    {:lvl 2 :gens {"b" (pg/cyc-notation [2 3])}}
                    {:lvl 3 :gens {"c" (pg/cyc-notation [3 4])}})}
        generators (BSGS-generators Sym4)
        sv-1 (Schreier-vector 1 generators)
        ]

    (is (= generators  ;; generators
            {"a" {2 1, 1 2}, "b" {3 2, 2 3}, "c" {4 3, 3 4}}))

    (is (= sv-1  ;;   Schreier-vector 
      {4 {:from 3, :gen "c"},
       3 {:from 2, :gen "b"},
       2 {:from 1, :gen "a"},
       1 {:gen nil, :from nil}}))

    ;; walk to root on Schreier-vector
    (is (= (Schreier-vector-point->root sv-1 4)  
           [{:point 2, :gen "a"} {:point 3, :gen "b"} 
            {:point 4, :gen "c"}]))

    ;; coset leader
    (is (= (Schreier-vector-coset-leader generators sv-1 4))  
        (pg/cyc 1 2 3 4))

    ;; Schreier-vector-chain
    (is (= (Schreier-vector-chain Sym4)
            {3 {4 {:from 3, :gen "c"}, 3 {:gen nil, :from nil}},
             2
             {4 {:from 3, :gen "c"},
              3 {:from 2, :gen "b"},
              2 {:gen nil, :from nil}},
             1
             {4 {:from 3, :gen "c"},
              3 {:from 2, :gen "b"},
              2 {:from 1, :gen "a"},
              1 {:gen nil, :from nil}}}))

    ;; check group membership
    (is (= (BSGS-check-group-membership 
             Sym4 (pg/cyc-notation [1 2] [3 4]))
           true))
    ))

(testing "BSGS Dihedral6"
  (let [Dihedral6 
        {:base (vector 1 2)
         :sgs (vector 
               {:lvl 1 :gens {"a" (pg/cyc 1 2 3 4 5 6)}}
               {:lvl 2 :gens {"b" (pg/cyc-notation [2 6] [3 5])
                              }})}]
    (is (true? (BSGS-check-group-membership 
                 Dihedral6 (pg/cyc-notation [1 2 3 4 5 6]))))
    (is (false? (BSGS-check-group-membership 
                  Dihedral6 (pg/cyc-notation [1 2 3 4]))))
    (is (true? (BSGS-check-group-membership 
                 Dihedral6 (pg/cyc-notation [2 6] [3 5]))))
    ))
    
(testing "BSGS Mathieu11"
  (let [Mathieu11 {:base (vector 1 2 3 4)
   :sgs (vector 
         {:lvl 1 :gens {"a" (pg/cyc-notation [1 10] [2 8] [3 11] [5 7])
                        "b" (pg/cyc-notation [1 4 7 6] [2 11 10 9])}}
         {:lvl 2 :gens {"c" (pg/cyc-notation [2 3] [4 5] [6 11] [8 9])}}
         {:lvl 3 :gens {"d" (pg/cyc-notation [3 5 7 9] [4 8 11 6])}}
         {:lvl 4 :gens {"e" (pg/cyc-notation [4 6] [5 11] [7 10] [8 9])
                        "f" (pg/cyc-notation [4 10 6 7] [5 9 11 8])
                        "g" (pg/cyc-notation [4 11 6 5] [7 8 10 9])
                            }})}]
    (is (false? (BSGS-check-group-membership 
                  Mathieu11 (pg/cyc-notation [1 2 3 4 5 6]))))
    (is (true? (BSGS-check-group-membership 
                Mathieu11 (pg/cyc-notation [4 11 6 5] [7 8 10 9])))
    )))

(testing "Schrier-Sims Mathieu11"
 (let [generating-set (vector 
         (pg/cyc-notation [1 10] [2 8] [3 11] [5 7]) 
         (pg/cyc-notation [1 4 7 6] [2 11 10 9]))
       p-bsgs {:base [1 2],
              :generating-set
              (vector {7 4, 1 6, 4 1, 6 7, 2 9, 11 2, 9 10, 10 11}
               {7 5, 1 10, 3 11, 2 8, 11 3, 5 7, 10 1, 8 2}
               {7 5, 1 10, 3 11, 2 8, 11 3, 5 7, 10 1, 8 2}
               {7 6, 1 4, 4 7, 6 1, 2 11, 11 10, 9 2, 10 9})}
       init-state (Schreier-Sims-initial-state p-bsgs)
       result (last (take-while (comp not nil?) 
               (iterate Schreier-Sims init-state)))
       sgs [{:sv
               {7 {:from 6, :gen "b"},
                1 {:gen nil, :from nil},
                4 {:from 1, :gen "c"},
                6 {:from 1, :gen "b"},
                3 {:from 11, :gen "a"},
                2 {:from 11, :gen "b"},
                11 {:from 10, :gen "b"},
                9 {:from 10, :gen "c"},
                5 {:from 7, :gen "a"},
                10 {:from 1, :gen "a"},
                8 {:from 2, :gen "a"}},
               :lvl 1,
               :gens
               {"a" {7 5, 1 10, 3 11, 2 8, 11 3, 5 7, 10 1, 8 2},
                "b" {7 4, 1 6, 4 1, 6 7, 2 9, 11 2, 9 10, 10 11},
                "c" {7 6, 1 4, 4 7, 6 1, 2 11, 11 10, 9 2, 10 9}}}
              {:sv
               {11 {:from 7, :gen "e"},
                8 {:from 3, :gen "d"},
                9 {:from 2, :gen "f"},
                7 {:from 4, :gen "f"},
                3 {:from 6, :gen "f"},
                5 {:from 4, :gen "d"},
                6 {:from 2, :gen "e"},
                4 {:from 2, :gen "d"},
                2 {:gen nil, :from nil}},
               :lvl 2,
               :gens
               {"d" {3 8, 4 5, 8 11, 6 2, 7 3, 5 6, 2 4, 11 7},
                "e" {7 11, 4 2, 6 5, 3 7, 2 6, 11 8, 5 4, 8 3},
                "f" {8 11, 9 2, 4 7, 11 8, 2 9, 6 3, 3 6, 7 4}}}
              {:sv
               {7 {:from 9, :gen "g"},
                4 {:from 3, :gen "i"},
                6 {:from 9, :gen "i"},
                3 {:gen nil, :from nil},
                11 {:from 4, :gen "i"},
                9 {:from 3, :gen "g"},
                5 {:from 3, :gen "h"},
                10 {:from 7, :gen "j"},
                8 {:from 3, :gen "j"}},
               :lvl 3,
               :gens
               {"g" {5 3, 8 4, 11 8, 3 9, 6 11, 7 5, 9 7, 4 6},
                "h" {6 4, 7 9, 5 7, 11 6, 9 3, 8 11, 4 8, 3 5},
                "i" {9 6, 8 3, 11 8, 6 10, 7 9, 4 11, 3 4, 10 7},
                "j" {7 10, 4 3, 11 4, 9 7, 10 6, 8 11, 3 8, 6 9}}}
              {:sv
               {7 {:from 4, :gen "n"},
                4 {:gen nil, :from nil},
                6 {:from 8, :gen "k"},
                11 {:from 9, :gen "m"},
                9 {:from 4, :gen "l"},
                5 {:from 9, :gen "n"},
                10 {:from 4, :gen "m"},
                8 {:from 4, :gen "k"}},
               :lvl 4,
               :gens
               {"k" {4 8, 6 9, 5 10, 9 4, 11 7, 8 6, 10 11, 7 5},
                "l" {5 7, 11 10, 6 8, 7 11, 4 9, 10 5, 9 6, 8 4},
                "m" {10 6, 7 4, 4 10, 5 9, 6 7, 9 11, 11 8, 8 5},
                "n" {5 8, 8 11, 11 9, 7 6, 9 5, 10 4, 4 7, 6 10}}}]]
     (is (= (partial-BSGS [1 2] [] generating-set)
            p-bsgs))
     (is (= (:sgs result) sgs))))
    
