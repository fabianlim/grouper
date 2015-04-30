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
               :generating-set (vector 
                {11 2, 10 11, 9 10, 2 9, 4 1, 7 4, 6 7, 1 6}
                {7 5, 5 7, 11 3, 3 11, 8 2, 2 8, 10 1, 1 10}
                {10 1, 1 10, 8 2, 2 8, 11 3, 3 11, 7 5, 5 7}
                {6 1, 7 6, 4 7, 1 4, 9 2, 10 9, 11 10, 2 11})}]
     (is (= (partial-BSGS [1 2] [] generating-set)
            p-bsgs))
    
     (let [init-state (Schreier-Sims-initial-state p-bsgs)
           ; gens (label-generators (:generating-set p-bsgs))
           ; sv (Schreier-vector 1 gens)
           ]
       ; (println "init")
       ; (clojure.pprint/pprint init-state)
       ;(println "alg")
       (clojure.pprint/pprint 
        (take 1 (iterate Schreier-Sims init-state)))
       )))

(let [gens {"b" {7 5, 1 10, 3 11, 2 8, 11 3, 5 7, 10 1, 8 2}, 
            "a" {7 4, 1 6, 4 1, 6 7, 2 9, 11 2, 9 10, 10 11}}
      sv {1 {:gen nil, :from nil}} ]
  ; (Schreier-generators gens sv)
  ; (Schreier-point->generators gens sv 1)
  )

; (BSGS-permutation-residue
;   {:sgs [{:lvl 2,
;     :gens
;     {"c" {4 7, 6 3, 3 6, 9 2, 8 11, 7 4, 2 9, 11 8},
;      "b" {3 8, 4 5, 8 11, 6 2, 7 3, 5 6, 2 4, 11 7},
;      "a" {7 11, 4 2, 6 5, 3 7, 2 6, 11 8, 5 4, 8 3}}
;     :sv
;       {4 {:from 2, :gen "b"},
;        6 {:from 2, :gen "a"},
;        2 {:gen nil, :from nil},
;        9 {:from 2, :gen "c"}}}]
;     :base [2]}
;    ; {7 10, 4 8, 2 11, 11 4, 9 5, 5 7, 10 9, 8 2}
;    {7 11, 4 2, 6 5, 3 7, 2 6, 11 8, 5 4, 8 3} 
;   )

(let [p-sgs
    {:sgs
     [{:sv {1 {:gen nil, :from nil}}, :lvl 1, :gens {}}
      {:sv
       {4 {:from 2, :gen "b"},
        6 {:from 2, :gen "a"},
        3 {:from 2, :gen "f"},
        2 {:gen nil, :from nil},
        11 {:from 2, :gen "d"},
        9 {:from 2, :gen "c"},
        8 {:from 2, :gen "e"}},
       :lvl 2,
       :gens
       {"f" {7 6, 4 7, 6 8, 3 5, 2 3, 11 2, 5 11, 8 4},
        "e" {2 8, 9 10, 7 5, 5 9, 4 11, 11 2, 8 4, 10 7},
        "d" {7 10, 4 8, 2 11, 11 4, 9 5, 5 7, 10 9, 8 2},
        "c" {4 7, 6 3, 3 6, 9 2, 8 11, 7 4, 2 9, 11 8},
        "b" {3 8, 4 5, 8 11, 6 2, 7 3, 5 6, 2 4, 11 7},
        "a" {7 11, 4 2, 6 5, 3 7, 2 6, 11 8, 5 4, 8 3}}}
      {:sv
       {3 {:gen nil, :from nil},
        5 {:from 3, :gen "h"},
        8 {:from 3, :gen "g"}},
       :lvl 3,
       :gens
       {"h" {7 10, 10 9, 6 8, 3 5, 9 11, 8 3, 5 6, 11 7},
        "g" {7 11, 6 5, 3 8, 11 9, 5 3, 8 6, 9 10, 10 7}}}],
     :base [1 2 3],
     :size 8,
     :level 2,
     :process-stack
     (vector (list 2 {4 9, 6 5, 3 10, 11 8, 9 4, 5 6, 10 3, 8 11})
      (list 2 {7 3, 4 5, 6 2, 3 8, 2 4, 11 7, 5 6, 8 11})
      (list 2 {4 5, 6 11, 3 2, 2 3, 11 6, 9 8, 5 4, 8 9})
      (list 2 {7 5, 4 11, 2 8, 11 2, 9 10, 5 9, 10 7, 8 4}))}
  ]
 (clojure.pprint/pprint (Schreier-Sims p-sgs))
;  (clojure.pprint/pprint (Schreier-Sims-new-gens p-sgs 2))
 )

; (let [generating-set (vector 
;              (pg/cyc-notation [1 10] [2 8] [3 11] [5 7]) 
;              (pg/cyc-notation [1 4 7 6] [2 11 10 9]))
;       p-bsgs (partial-BSGS [1 2] [] generating-set)
;       ; sv (Schreier-vector 1 gens)
;       ]
;   ; (clojure.pprint/pprint 
;   ;   (assoc p-bsgs :sgs (vector) :sgs-build {:lvl 1 :gens {} :size 0}))
;   (clojure.pprint/pprint 
;     (take 20 (iterate Schreier-procedure 
;       (assoc p-bsgs :sgs (vector) :sgs-build {:lvl 1 :gens {} :size 0}))
;      ))
;   )
; 
; 
; (let [generating-set (vector 
;              (pg/cyc-notation [1 2]) 
;              (pg/cyc-notation [2 3])
;              (pg/cyc-notation [3 4])
;              )
;       p-bsgs (partial-BSGS [1 2] [] generating-set)
;       base (:base p-bsgs)]
;   (clojure.pprint/pprint 
;     (take 20 (iterate Schreier-procedure 
;       (assoc p-bsgs :sgs (empty-sgs 1 (count base)) :index 0)))



;; (def T (partial-BSGS [] [] (vector (pg/cyc-notation [1 2])
;;                             (pg/cyc-notation [2 3])
;;                             (pg/cyc-notation [3 4])
;;                             )))

; (clojure.pprint/pprint (take 4 (iterate Schreier-procedure 
;   {:base [] :sgs []
;    :base-stabilizers (label-generators (vector (pg/cyc 1 2) (pg/cyc 2 3) (pg/cyc 3 4)))})))


;; (let [gens {"a" (pg/cyc-notation [1 10] [2 8] [3 11] [5 7]) 
;;            "b" (pg/cyc-notation [1 4 7 6] [2 11 10 9])}
;;       sv (Schreier-vector 1 gens)
;;       ]
;;   (clojure.pprint/pprint (nth (iterate Schreier-procedure 
;;     {:base [] :sgs []
;;      :base-stabilizers gens}) 3))
;;   ; (clojure.pprint/pprint (keys sv))
;;   ; (clojure.pprint/pprint sv)
;;   ; (clojure.pprint/pprint (Schreier-point->generators gens sv 2))
;;   ; (clojure.pprint/pprint (Schreier-generators gens sv))
;;   ; (Schreier-vector-coset-leader gens sv 9)
;;   ; (Schreier-vector-point->root sv 9)
;;   )

; (clojure.pprint/pprint T)
; (let [{:keys [base sgs]} T]
;   (order-gens-wrt-base base sgs))
