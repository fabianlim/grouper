;; schreier-sims
;; TODO: implement for permutation groups first
(ns grouper.schreier-sims
  (:require [grouper.permutation-groups :as pg]))

;; int->key
;; private helper function
(defn- int->key 
  [n]
  (str (char (+ 96 n))))

;; extend-Schreier-vector
;; actually a BFS 
;; TODO: assumes points are indices
(defn extend-Schreier-vector
  [init-tree generating-set]
  " extend Schreier tree from init-tree
  " 
  (loop [tree init-tree
         queue (conj clojure.lang.PersistentQueue/EMPTY 
                     (keys init-tree))]
      (if (empty? queue)
        tree
        (let [x (peek queue)  ;; current item
          new-points (->> generating-set
            (map (fn [[n gen]] (vector (gen x) n)))
            (filter #(first %)) ;; filter out points that are nil 
            ;; filter points not in tree
            (filter #(not (contains? tree (first %)))))]  
          (recur (reduce #(assoc %1 (first %2) 
             {:from x :gen (second %2)}) tree new-points) 
             (reduce conj (pop queue) (map first new-points)))))))

;; Schreier-vector
(defn Schreier-vector
  " compute Schreier vector 

    Inputs
    ======
    * root: point to start from
    * generating-set: map of generators (permutations)
      eg. {'a' {2 1, 1 2}, 'b' {3 2, 2 3}, 'c' {4 3, 3 4}}
  " 
  [root generating-set]
  (extend-Schreier-vector 
    {root {:from nil :gen nil}} generating-set))

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

;; Schreier-vector-point->root
(defn Schreier-vector-point->root
  [sv point]
  " returns Schreier vector traceback from point to root 

    e.g. [{:point 3, :gen 'b'} {:point 2, :gen 'a'}]
  "
  (loop [x point
         path (vector)]
    (if (nil? (:from (sv x)))  ;; returns empty vector if x not in sv
      path
      (let [{:keys [gen from]} (sv x)]
        (recur from (cons {:point x :gen gen} path))))))

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
       (Schreier-vector-point->root sv)
       (map :gen)
       (reduce #(pg/compose %1 (generators %2)) {})))

;; BSGS-perm-residue
(defn BSGS-permutation-residue
  [{:keys [sgs base]} perm]
  " find the residue of a permutation with respect to 
    a bsgs

    Input
    =====
    sgs: strong-generating set with keys 
         [:lvl, :gens, :sv (optional)], e.g.,
    {:lvl 2,
     :gens
     {'c' {4 7, 6 3, 3 6, 9 2, 8 11, 7 4, 2 9, 11 8},
      'b' {3 8, 4 5, 8 11, 6 2, 7 3, 5 6, 2 4, 11 7},
      'a' {7 11, 4 2, 6 5, 3 7, 2 6, 11 8, 5 4, 8 3}}
     :sv  (optional: computes if missing)
       {4 {:from 2, :gen 'b'},
        6 {:from 2, :gen 'a'},
        2 {:gen nil, :from nil},  <- base point
        9 {:from 2, :gen 'c'}}}]
    base: a vector of base points. This cannot be derived
          from :sv above since that is an optional 
          argument
  "
  (loop [g perm 
         [b & bs] base  ; base element
         S sgs
         lvl (:lvl (sgs 0))]  ;; TODO get head of sgs
    (if (or (empty? g) (nil? b))
      [g lvl]
      (let [x (get g b b)  ;; image of b under g
            gens (SGS-generators S)
            sv (or (get-in S [0 :sv])
                   (Schreier-vector b gens))
            cl (Schreier-vector-coset-leader 
                 gens sv x)]
        (if-not (contains? sv x)
          [g lvl]
          (recur (pg/compose g (pg/inverse cl)) bs
            ; (filter #(> (:lvl %) lvl) S)    
            (subvec S 1)
            (inc lvl)))))))

;; BSGS-check-group-membership
(defn BSGS-check-group-membership
  [bsgs perm]
  " check group membership using BSGS " 
  (empty? (first (BSGS-permutation-residue bsgs perm))))

;; BSGS-random-element
(defn BSGS-random-element
  [{:keys [base sgs] :as bsgs}]
  (let [sv-chain (Schreier-vector-chain bsgs)
        generators (SGS-generators sgs)
        select (fn [x] 
                 (Schreier-vector-coset-leader generators
                   (sv-chain x) (rand-nth (keys (sv-chain x)))))]
    (reduce pg/compose (map select (reverse base)))))

; TODO: will ignore remainder of set if there are more than 
; 26 generators in that set
(defn- label-generators
  " utility function to label generators using
    alphabets a-z 
    
    Inputs
    ======
    generator-set: vector of generators 
    label-start: numerical offset of starting label (starts from 1)
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
                  (assoc labeled-gens s (int->key n))))))
  ([generating-set]
   (label-generators generating-set 1)))

; order-gens-wrt-base
(defn order-gens-wrt-base 
  [base gens]
  " label generators to which level they belong to " 
  (loop [[b & bs] base
          B []
          S (set gens)
          i 1
          sz 1
          ord-gens (vector)]
    (if (nil? b) 
      ord-gens
      (let [B-ext (conj B b) ;; extend 
            gens (clojure.set/select
                   #((comp not pg/pointwise-stabilizer?) % B-ext) S)]
        (recur bs 
               B-ext
               (clojure.set/difference S gens)
               (inc i)
               (+ sz (count gens))
               (conj ord-gens 
                 {:lvl i 
                  :gens (label-generators (seq gens) sz)}))))))

;; partial-BSGS
(defn partial-BSGS
  [base-points SGS-perms generating-set]
  " compute a partial-BSGS

    a partial-BSGS satisfies the following:
    * contains a generating set
    * closed under inversion
    * all permutations must not fix (pointwise?) every element in 
      the base

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
        {:base B :generating-set S}
        (let [base-as-set (set B)]
          (recur ss 
                 (cons (pg/inverse s) S)
                 ;; check if need to extend base
                 (if (pg/pointwise-stabilizer? s B)  
                   (conj B
                     (first (filter 
                              #(not (contains? base-as-set %)) (keys s))))
                   B)))))))

; Scherier0
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
        coset-leader ;; note: this gives element mapping x->base
          (fn [x] (Schreier-vector-coset-leader generators sv x))
        coset-leader-point (coset-leader point)]
    (filter (comp not empty?) ;; take out identity elements
      (map (fn [s image-s] 
            (-> (pg/compose coset-leader-point s)
                (pg/compose (pg/inverse (coset-leader image-s)))))
           (vals generators) images))))

(defn Schreier-generators
  [generators sv]
  " compute all the Schreier generators 
    using the orbit from a Scherier vector 

    Inputs
    ======
    generators: hash-map of generators
      e.g. {'a' {2 1, 1 2}, 'b' {4 3, 3 4}, 'c' {3 2, 2 3}}
    sv: Schreier-vector w.r.t. generators and some base
  "
  (reduce  ;; (keys sv) is really the orbit
    #(into %1 (Schreier-point->generators 
                generators sv %2)) #{} (keys sv)))

(defn- empty-sgs 
  ([start end]
  " build an empty sgs from levels start to end "
    (->> (range start (inc end))
      (map #(hash-map :lvl % :gens (hash-map)))
      (reduce conj (vector))
    ))
  ([x]
   (first (empty-sgs x (inc x)))))

; (defn- initialize-process-stack 
;   [gen-set]
;   (list (hash-map :lvl 1 :stack (seq gen-set))  ))

(defn- extends-Schreier-vector?
  [sv node from]
  (and (not (contains? sv node))  ;; node not yet there
          (contains? sv from))) ;; from is there

; (defn- update-Schreier-vector
;   [sv node from gen]
;   " doesn't check the gen labeling though " 
;   (if (extends-Schreier-vector? sv node from)
;     (assoc sv node {:from from :gen gen})
;     sv))

(defn- Schreier-vector-chain-stump
  [b]
  (hash-map b {:gen nil :from nil}))

;; build-Scherier-procedure
(defn build-Scherier-procedure
  [procedure-check-completion
   procedure-find-new-generators
   procedure-update-stack
   procedure-consume-generator]
 (fn [p-bsgs]
  " state-machine builder of Schreier algorithms "
   (cond (nil? p-bsgs)
      nil
    (procedure-check-completion p-bsgs)
      (let [b-stabs (procedure-find-new-generators p-bsgs)]  
        (if (empty? b-stabs) ;; if no more stabilizers 
          nil ; TODO: perhaps I will just terminate here 
          (procedure-update-stack p-bsgs b-stabs)))
    :else
     (procedure-consume-generator p-bsgs))))

;; update-if
(defn- update-if
  [m pred x & xs]
  " update map m when pred is true " 
  (if-not pred
    m 
    (apply x m xs)))

;; partial-bsgs-insert-generator
(defn- partial-bsgs-insert-generator
  [p-bsgs drop-out generator]
  " insert generator at the drop-out " 
  (let [b (get-in p-bsgs [:base (dec drop-out)]) ;; base-point
      k (int->key (inc (:size p-bsgs)))  ;; key
      image (generator b)  ;; image
      ; sv (get-in p-bsgs [:sv-chain b]) ;; Scherier- vector
      sv (get-in p-bsgs [:sgs (dec drop-out) :sv]) ;; Scherier- vector
      new-gen? (extends-Schreier-vector? sv image b)]
    (do 
      (println "sv" sv)
      (println "do" drop-out)
      (println "gen" generator)
      (println "new-gen" new-gen?)
      (println "image" image)
      (println "b " b)
      (-> p-bsgs 
        ; (update-if new-gen? assoc-in [:sv-chain b image] 
        ;    {:from b :gen k})
        (update-if new-gen? assoc-in [:sgs (dec drop-out) :sv image] 
           {:from b :gen k})
        (update-if new-gen? update-in [:size] inc)
        (update-if new-gen? assoc-in 
           [:sgs (dec drop-out) :gens k] generator)))))

;; TODO : which gens are new?
(defn Schreier-Sims-new-gens
  [{:keys [sgs base] :as p-bsgs} up-level]
  (let [gens (->> (filter #(>= (:lvl %) up-level) sgs)
                 (reduce #(into %1 (:gens %2)) {}))
        base-up (base (dec up-level))
        sv (Schreier-vector base-up gens)
        Gstabs (Schreier-generators gens sv) ;; last level stabs to init
        ]
    sv))

;; Schreier-Sims-initial-state
(defn Schreier-Sims-initial-state
  [{:keys [base generating-set]}]
  (let 
    [level (count base)
     gens (label-generators generating-set)
     base-up (base (dec (dec level)))
     sv (Schreier-vector base-up gens)
     Gstabs (Schreier-generators gens sv) ;; last level stabs to init
     ]
    {; :sgs (reduce #(into) (empty-sgs 1 level))
     :sgs (into [] (map #(assoc %1 :sv 
             (Schreier-vector-chain-stump %2)) 
             (empty-sgs 1 level) base))
     :base base
     :size 0
     :level level  ; start from bottom
     ; :sv-chain (into {} 
     ;     (map #(vector % 
     ;     (Schreier-vector-chain-stump %)) base))  ;; init empty
     ; :process-stack (map #(list level %) generating-set)
     :process-stack (map #(list level %) Gstabs)
     }))

;; Scherier-Sims-update-after-consumption
(defn- Scherier-Sims-update-after-consumption 
  [{:keys [base size] :as p-bsgs} drop-out residue]
  " update the sgs " 
  (if (empty? residue)
    p-bsgs  ;; empty residue, do nothing
    (let [extend-base? (> drop-out (count base))
          b (if-not extend-base?
              (base (dec drop-out))
              ;(first (pg/moved-points-other-than residue base))
              (min-moved-point-not-in-set [residue] base))
              ]
      (do
        (println "extend-base" extend-base?)
        (-> p-bsgs 
            ; (update-if extend-base? update-in  ;; TODO : fix
            ;   [:sv-chain] conj {b (Schreier-vector-chain-stump b)})
            (update-if extend-base? update-in 
              [:sgs] conj {:sv (Schreier-vector-chain-stump b)
                           :lvl drop-out :gens {}})
            (update-if extend-base? update-in [:base] conj b)
            ; (update-if extend-base? update-in [:sgs] conj
            ;   {:lvl drop-out :gens {}})
            (partial-bsgs-insert-generator drop-out residue)
            (partial-bsgs-insert-generator drop-out (pg/inverse residue))
            )))))

;; Schreier-Sims
(def Schreier-Sims
   (letfn 
     [(subbase-stabilizers
       [perm-seq base index]
       " filter out elements in perm-seq that stabilize 
         subbase(0,index-1) 
       " 
       (filter #(pg/pointwise-stabilizer? % 
          (subvec base 0 index) perm-seq)))
     (completion 
       [{:keys [process-stack level]}]
       (or (empty? process-stack) )
           (not= (first (first process-stack)) level))
     (find-new 
       [{:keys [sv-chain base level] :as p-bsgs}]
       " the new addition in S plus the new coset rep " 
       (when-not (= level 1)
         (let
           [up-level (dec level)  ;; dec level
            b (base (dec up-level)) ;; base one level below
            sv (sv-chain b)  ;; Schreier-vector one leve up
            ;; TODO: should only need to consider gens from 
            ;; prev level. Should be the only new gens
            gens (get-in p-bsgs [:sgs (dec level) :gens])]
           (do 
             (println)
             (println)
             (println "gens" gens)
             (println "sv" sv)
             (Schreier-generators gens sv)))))
     (update-stack  ;; go up
       [{:keys [index sgs base] :as p-bsgs}] 
       ;; if completed and there are some b-stabs
       ;; need to go up
       (when (> 0 index)
         (-> p-bsgs 
           (update-in [:index] dec)
           (update-in [:generating-set] cons
             (subbase-stabilizers sgs base index)))))
     (consume-generator
       [{:keys [process-stack sgs base] :as p-bsgs}]
       " get the residue and update " 
       (let [[[l g] & gs] process-stack
              subv #(subvec % (dec l) (count base))
              [g-res drop-out] (BSGS-permutation-residue
                  {:sgs (subv sgs) :base (subv base)} g)]
         (do
         (println {:sgs (subv sgs) :base (subv base)})
         (-> p-bsgs
            ; update sv-chain, sgs, base
           (Scherier-Sims-update-after-consumption drop-out g-res)
           ;; TODO update level if going down
           (assoc :process-stack gs)))))]

     (build-Scherier-procedure 
       completion
       find-new
       update-stack
       consume-generator)))

; (BSGS-permutation-residue
;   {:sgs [{:lvl 1 :gens {}}] :base [1 2]}
;   (pg/cyc-notation [3 4]))

; Schreier-procedure
(def Schreier-procedure
  " top-to-bottom approach " 
  (letfn [ 
    (find-new
      [{:keys [index base sgs]}]
      (let [b (base index) ;; this base
           subbase (subvec base 0 (inc index))
           gens (->> sgs
             (filter #(pg/pointwise-stabilizer? % subbase))
             (label-generators))
           sv (Schreier-vector b gens)] ;; construct sv
         ;; stabilizer w.r.t. b
         (seq (Schreier-generators gens sv))))
    (update 
      [x new-gens] 
      (-> x
        ;; go down one level
        (update-in [:index] inc)
        ;; stack will be empty, so put in new-gens
        (assoc :process-stack new-gens)))   
    (consume 
      [{:keys [process-stack base] :as p-bsgs}]
      (let [[g & gs] process-stack   ;; if generating set not empty
             ;; get a b not contained
             b (first (pg/moved-points-other-than g base))
             extend-base? (pg/pointwise-stabilizer? g base)
             perform-extension (fn [x y] (if extend-base? (conj x y) x))]
         (-> p-bsgs 
             (assoc :process-stack gs)  ;; pop process-stack
             (update-in [:base] perform-extension b)
             (update-in [:sgs] into [g (pg/inverse g)]))))]
    (build-Schreier-procedure 
      #(empty? (:process-stack %))
      find-new update consume)))

; Schreier-procedure-initial-state
(defn Schreier-procedure-initial-state
  [p-bsgs]
  (-> p-bsgs
    (clojure.set/rename-keys {:generating-set :process-stack})
    (assoc :sgs #{} :index 0)))

(defn- min-moved-point-not-in-set
 [perms points] 
 " pick the minimum moved point by elements in perms not 
   in the set of points
 "
 ;; function to reject those in base
 (first (reduce #(vector (apply min 
   (concat %1 (pg/moved-points-other-than %2 points)))) nil perms)))

(defn Schreier-procedure
  [{:keys [base sgs base-stabilizers]}]
  " 
    Inputs
    ======
    a hash-map containing the following keys:
    - base: set of base points, e.g. []
    - sgs:  strong generating set
    - base-stabilizers: hash-map of generators that are 
      pointwise-stabilizers of base, to be included
        into the BSGS
        e.g. {'a' {2 1, 1 2}, 'b' {4 3, 3 4}, 'c' {3 2, 2 3}}
  " 
  (let [b (min-moved-point-not-in-set 
            (vals base-stabilizers) base) ;; extend to this point
        ;; compute Schreier-vector of gens wrt b
        sv (Schreier-vector b base-stabilizers)  
        lvl-gens ;new level gens
          (filter #(not 
           (pg/pointwise-stabilizer? 
             (second %) [b])) base-stabilizers)  
        ;; compute Schreier-generators of new stabilizer
        Gstab (Schreier-generators 
                base-stabilizers sv)  
        gen-label 
          (apply min (map #(- (int (.charAt % 0)) 96) 
                          (keys base-stabilizers)))
        new-gen-label (+ gen-label (count lvl-gens))]
    {:base 
      (if (nil? b) base (conj base b))
     :sgs ;; have to relabel the gens
      (conj sgs {:lvl (inc (count base)) 
                 :gens (label-generators 
                         (vals lvl-gens) gen-label)})  
     :base-stabilizers (label-generators (seq Gstab) new-gen-label)}))
