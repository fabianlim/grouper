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

    (is (= sv-1  ;;   Schrerier-vector 
      {4 {:from 3, :gen "c"},
       3 {:from 2, :gen "b"},
       2 {:from 1, :gen "a"},
       1 {:gen nil, :from nil}}))

    ;; walk to root on Schrerier-vector
    (is (= (Schrerier-vector-point->root sv-1 4)  
           [{:point 4, :gen "c"} {:point 3, :gen "b"} {:point 2, :gen "a"}]))

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
  (let [Dihedral6 {:base (vector 1 2)
                   :sgs (vector 
                         {:lvl 1 :gens {"a" (pg/cyc 1 2 3 4 5 6)}}
                         {:lvl 2 :gens {"b" (pg/cyc-notation [2 6] [3 5])
                                            }})}
        ]
    (is (true? (BSGS-check-group-membership Dihedral6 (pg/cyc-notation [1 2 3 4 5 6])))
    (is (false? (BSGS-check-group-membership Dihedral6 (pg/cyc-notation [1 2 3 4])))
                    ))))
    
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
    (is (false? (BSGS-check-group-membership Mathieu11 (pg/cyc-notation [1 2 3 4 5 6]))))
    (is (true? (BSGS-check-group-membership Mathieu11 (pg/cyc-notation [4 11 6 5] [7 8 10 9])))
    ;; (is (false? (BSGS-check-group-membership Dihedral6 (pg/cyc-notation [1 2 3 4])))
                    )))
