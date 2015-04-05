;; schreier-sims
;; TODO: implement for permutation groups first
(ns grouper.schreier-sims
  (:require [grouper.permutation-groups :as pg]))

;; Schreier-vector
;; actually a BFS 
;; TODO: assumes points are indices
(defn Schreier-vector
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
            (map (fn [[n gen]] (vector (gen x) n)))
            (filter #(first %)) ;; filter out items that are nil 
            (filter #(not (contains? tree (first %)))))]  ;; filter items not in tree
          (recur (reduce #(assoc %1 (first %2) 
             {:from x :gen (second %2)}) tree new-items) 
             (reduce conj (pop queue) (map first new-items)))))))

;; SGS-generators
(defn SGS-generators [sgs]
  (reduce #(into %1 (:gens %2)) {} sgs))

;; BSGS-generators
(defn BSGS-generators [bsgs]
  (SGS-generators (:sgs bsgs)))

;; Schreier-vector-chain
(defn Schreier-vector-chain 
  [{:keys [base sgs]}]
  " construct chain of Schreier vectors for a bsgs 

    Inputs
    ======
    e.g. 
    base: [1 2 3]
    sgs:  [{:lvl 1 :gens {'a' (pg/cyc-notation [1 2])}}
           {:lvl 2 :gens {'b' (pg/cyc-notation [2 3])}}
           {:lvl 3 :gens {'c' (pg/cyc-notation [3 4])}}]
    Outputs
    =======
    - chain of Schreier vectors
      {3 {4 {:from 3, :gen 'c'}, 3 {:gen nil, :from nil}},
       2
       {4 {:from 3, :gen 'c'},
        3 {:from 2, :gen 'b'},
        2 {:gen nil, :from nil}},
       1
       {4 {:from 3, :gen 'c'},
        3 {:from 2, :gen 'b'},
        2 {:from 1, :gen 'a'}, 1 {:gen nil, :from nil}}}
  " 
  (loop [n 1
         [b & bs] base
         sv {}
         X sgs]
    (if (nil? b)
      sv
      (recur (inc n) bs
             (assoc sv b (Schreier-vector b
                (SGS-generators X)))
             (filter #(> (:lvl %) n) X)))))

;; Schrerier-vector-point->root
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

;; Schreier-vector-coset-leader
(defn Schreier-vector-coset-leader
  [generators sv point]
  " compute the Schreier-vector coset leader 

    Inputs
    ======
    generators: hash-map of generators
      e.g. {'a' {2 1, 1 2}, 'b' {4 3, 3 4}, 'c' {3 2, 2 3}}
    sv: Schreier-vector w.r.t. generators and some base
    point: this point is moved under action of generators
  " 
  (->> point
       (Schrerier-vector-point->root sv)
       (map :gen)
       (reduce #(pg/compose %1 (generators %2)) {})))


;; BSGS-check-group-membership
(defn BSGS-check-group-membership
  [{:keys [sgs base]} perm]
  " check group membership using BSGS " 
  (loop [g perm 
         [b & bs] base  ; base element
         S sgs
         lvl 1]
    (cond (empty? g) 
        true
      (nil? b)
        false
      :else
        (let [x (get g b b)  ;; image of b under g
              generators (SGS-generators S)  
              sv (Schreier-vector b generators)
              cl (Schreier-vector-coset-leader generators sv x) ]
          (if-not (contains? sv x)
            false
            (recur (pg/compose (pg/inverse cl) g) bs
              (filter #(> (:lvl %) lvl) S)    
              (inc lvl)))))))

;; BSGS-random-element
(defn BSGS-random-element
  [{:keys [base sgs] :as bsgs}]
  (let [sv-chain (Schreier-vector-chain bsgs)
        generators (SGS-generators sgs)
        select (fn [x] 
                 (Schreier-vector-coset-leader generators
                   (sv-chain x) (rand-nth (keys (sv-chain x)))))]
    (reduce pg/compose (map select (reverse base)))))


(defn order-gens-wrt-base 
  [base gens]
  " label generators to which level they belong to " 
  (loop [[b & bs] base
          B []
          S (set gens)
          i 1
          ord-gens (vector)]
    (if (nil? b) 
      ord-gens
      (let [B-ext (conj B b) ;; extend 
            gens (clojure.set/select #(pg/pointwise-stabilizer? % B-ext) S)]
        (recur bs 
               B-ext
               (clojure.set/difference S gens)
               (inc i)
               (conj ord-gens {:lvl i :gens gens}))))))

;; partial-BSGS
(defn partial-BSGS
  [base-points SGS-perms generating-set]
  " compute a partial-BSGS

    a partial-BSGS satisfies the following:
    * contains a generating set
    * closed under inversion
    * all permutations must not fix (pointwise?) every element in the base

    Inputs
    ======
    base-points: vector of points to include in the base
    SGS-perms: set of permutations to include in the SGS
    generating-set: generating set that the partial-BSGS will contain
  " 
  (let [T (->> (clojure.set/union (set SGS-perms) (set generating-set))
                (remove #(= % {})))] ;; remove identity perm
    (loop [[s & ss] T
            S T
            B base-points]
      (if (nil? s)
        {:base B :sgs (distinct S)}
        (let [base-as-set (set B)]
          (recur ss 
                 (cons (pg/inverse s) S)
                 (if (pg/pointwise-stabilizer? s B)  ;; check if need to extend base
                   (conj B
                     (first (filter #(not (contains? base-as-set %)) (keys s))))
                   B)))))))


; (clojure.pprint/pprint T)
; (let [{:keys [base sgs]} T]
;   (order-gens-wrt-base base sgs))

; TODO: will ignore remainder of set if there are more than 
; 26 generators in that set
(defn- label-generators
  " utility function to label generators using
    alphabets a-z 
    
    Inputs
    ======
    generator-set: vector of generators 
    label-start: string of starting label, e.g. 'a', 'b', etc
  "
  ([generator-set label-start]
    (loop [n label-start
           [s & ss] generator-set
           labeled-gens (hash-map)]
       (cond 
         (or (nil? s) (> n 26))
           (clojure.set/map-invert labeled-gens)
         (contains? labeled-gens s)  ;; skip non-unique gens
           (recur n ss labeled-gens)
         :else
           (recur (inc n) ss
                  (assoc labeled-gens s (str (char (+ 96 n))))))))
  ([generating-set]
   (label-generators generating-set 1)))

(defn Schreier-point->generators
  [generators sv point]
  " compute the Schreier generators under action of generators on point 
    
    Inputs
    ======
    generators: hash-map of generators
      e.g. {'a' {2 1, 1 2}, 'b' {4 3, 3 4}, 'c' {3 2, 2 3}}
    sv: Schreier-vector w.r.t. generators and some base
    point: this point is moved under action of generators
  " 

  (let [images (map #(% point point) (vals generators))
        coset-leader ;; not this gives element mapping x->base
          (fn [x] (Schreier-vector-coset-leader generators sv x))
        coset-leader-point ((comp pg/inverse coset-leader) point)]
    (filter (comp not empty?) ;; take out identity elements
      (map (fn [s image-s] 
            (-> (pg/compose coset-leader-point s)
                (pg/compose (coset-leader image-s))))
           (vals generators) images))))

(defn Schreier-generators
  [generators sv]
  " compute all the Schreier generators using the orbit from a Scherier vector 

    Inputs
    ======
    generators: hash-map of generators
      e.g. {'a' {2 1, 1 2}, 'b' {4 3, 3 4}, 'c' {3 2, 2 3}}
    sv: Schreier-vector w.r.t. generators and some base
  "
  (reduce  ;; (keys sv) is really the orbit
    #(into %1 (Schreier-point->generators generators sv %2)) #{} (keys sv)))


;; (def T (partial-BSGS [] [] (vector (pg/cyc-notation [1 2])
;;                             (pg/cyc-notation [2 3])
;;                             (pg/cyc-notation [3 4])
;;                             )))


; (clojure.pprint/pprint (take 4 (iterate Schreier-procedure 
;   {:base [] :sgs []
;    :base-stabilizers (label-generators (vector (pg/cyc 1 2) (pg/cyc 2 3) (pg/cyc 3 4)))})))

(defn Schreier-procedure
  [{:keys [base sgs base-stabilizers]}]
  " 
    Inputs
    ======
    base: set of base points, e.g. []
    base-stabilizers hash-map of generators that are pointwise-stabilizers of base
      e.g. {'a' {2 1, 1 2}, 'b' {4 3, 3 4}, 'c' {3 2, 2 3}}
  " 
  (let [[label perm] (first base-stabilizers)
        b (first (keys perm))  ;; will extend base to point b
        sv (Schreier-vector b base-stabilizers)  ;; compute Schreier-vector of gens wrt b
        lvl-gens (filter #(not (pg/pointwise-stabilizer? (second %) [b])) base-stabilizers)  ;new level gens
        Gstab (Schreier-generators base-stabilizers sv)  ;; compute Schreier-generators of new stabilizer
        gen-label (apply min (map #(- (int (.charAt % 0)) 96) (keys base-stabilizers)))
        new-gen-label (+ gen-label (count lvl-gens))
        ]
    {:base 
      (if (nil? b) base (conj base b))
     :sgs 
      (conj sgs {:lvl (inc (count base)) 
                 :gens (label-generators (vals lvl-gens) gen-label)})  ;; have to relabel the gens
     :base-stabilizers (label-generators (seq Gstab) new-gen-label)
     }))
