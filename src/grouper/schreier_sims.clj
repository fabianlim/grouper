;; schreier-sims
;; TODO: implement for permutation groups first
(ns grouper.schreier-sims
  (:require [grouper.permutation-groups :as pg]))

(def Sym4 {:base (vector 1 2 3)
           :sgs {:fixes-base nil :gens (vector (pg/cyc-notation [1 2]))
                 :next 
                   {:fixes-base 1 :gens (vector (pg/cyc-notation [2 3]))
                    :next
                      {:fixes-base 2 :gens (pg/cyc-notation [3 4])
                       :next nil}}}})

;; compute-Schreier-vector
;; actually a BFS 
;; TODO: assumes points are indices
(defn compute-Schreier-vector
  [root generating-set]
  " compute Schreier vector 

    Inputs
    ======
    * root: point to start from
    * generating-set: map of generators (permutations)
      eg. {'a' {2 1, 1 2}, 'b' {3 2, 2 3}, 'c' {4 3, 3 4}}
  " 
  (loop [tree {root {:from nil :gen nil}}
         queue (conj clojure.lang.PersistentQueue/EMPTY root)]
      (if (empty? queue)
        tree
        (let [x (peek queue)  ;; current item
          new-items (->> generating-set
            ;; (map-indexed (fn [i gen] (vector (gen x) i)))  ;; map [item gen-index] pairs
            (map (fn [[n gen]] (vector (gen x) n)))
            (filter #(first %)) ;; filter out items that are nil 
            (filter #(not (contains? tree (first %)))))]  ;; filter items not in tree
          (recur (reduce #(assoc %1 (first %2) 
             {:from x :gen (second %2)}) tree new-items) 
             (reduce conj (pop queue) (map first new-items)))))))

(def Sym4 {:base (vector 1 2 3)
           :sgs (vector 
                  {:lvl 1 :gens {"a" (pg/cyc-notation [1 2])}}
                  {:lvl 2 :gens {"b" (pg/cyc-notation [2 3])}}
                  {:lvl 3 :gens {"c" (pg/cyc-notation [3 4])}}
                  )})

; (compute-Schreier-vector 1 (map :gens (:sgs Sym4)))
; (compute-Schreier-vector 2 (reduce #(into %1 (:gens %2)) {} (:sgs Sym4)))

(let [x 1
      t (reduce #(into %1 (:gens %2)) {} (:sgs Sym4))]
  ; (map-indexed (fn [i gen] (vector (gen x) i)) t)  ;; map [item gen-index] pairs
  (map (fn [[n gen]] (vector (gen 1) n)) t)
  )

;; SGS-generators
(defn SGS-generators [sgs]
  (reduce #(into %1 (:gens %2)) {} sgs))

;; BSGS-generators
(defn BSGS-generators [bsgs]
  (SGS-generators (:sgs bsgs)))

;; compute-Schreier-vector-chain
(defn compute-Schreier-vector-chain 
  [{:keys [base sgs]}]
  " construct chain of Schreier vectors for a bsgs 

    e.g. for Sym4 with base (1 2 3) and sgs [(1, 2), (2, 3), (3, 4)]
      
      - chain of Schreier vectors
        {3 {4 {:from 3, :gen "c"}, 3 {:gen nil, :from nil}},
         2
         {4 {:from 3, :gen "c"},
          3 {:from 2, :gen "b"},
          2 {:gen nil, :from nil}},
         1
         {4 {:from 3, :gen "c"},
          3 {:from 2, :gen "b"},
          2 {:from 1, :gen "a"}, 1 {:gen nil, :from nil}}}
  " 
  (loop [n 1
         [b & bs] base
         sv {}
         X sgs]
    (if (nil? b)
      sv
      (recur (inc n) bs
             (assoc sv b (compute-Schreier-vector b
                (SGS-generators X)))
             (filter #(> (:lvl %) n) X)))))

(defn Schrerier-vector-point->root
  [sv point]
  " returns Schrerier vector traceback from point to root 

    e.g. [{:point 3, :gen 'b'} {:point 2, :gen 'a'}]
  "
  (loop [x point
         path (vector)]
    (if (nil? (:from (sv x)))  ;; returns empty vector if x not in sv
      path
      (let [{:keys [gen from]} (sv x)]
        (recur from (conj path {:point x :gen gen}))))))

;; {"a" {2 1, 1 2}, "b" {3 2, 2 3}, "c" {4 3, 3 4}}
; (let [svc (compute-Schreier-vector-chain Sym4)
;       generators (BSGS-generators Sym4)
;       [base point] [1 2]
;       sv (svc base)]
;   (Schreier-vector-coset-leader generators sv point))  

;; Schreier-vector-coset-leader
(defn Schreier-vector-coset-leader
  [generators sv point]
  " compute the Schreier-vector coset leader " 
  (->> point
       (Schrerier-vector-point->root sv)
       (map :gen)
       (reduce #(pg/compose %1 (generators %2)) {})))

(BSGS-check-group-membership Sym4 (pg/cyc-notation [1 2] [3 4]))

;; BSGS-check-group-membership
(defn BSGS-check-group-membership
  [{:keys [sgs base]} perm]
  " check group membership using BSGS " 
  (loop [g perm 
         [b & bs] base  ; base element
         S sgs
         lvl 1]
    (if (empty? g) 
      true
      (let [x (get g b b)  ;; image of b under g
            generators (SGS-generators S)  
            sv (compute-Schreier-vector b generators)
            cl (Schreier-vector-coset-leader generators sv x) ]
        (println "x" x "g" g "sv" sv)
        (if-not (contains? sv x)
          false
          (recur (pg/compose (pg/inverse cl) g) bs
            (filter #(> (:lvl %) lvl) S)    
            (inc lvl)))))))
