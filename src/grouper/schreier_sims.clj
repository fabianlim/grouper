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
         queue (apply conj clojure.lang.PersistentQueue/EMPTY 
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
(defn SGS-generators 
  ([sgs]
    (reduce #(into %1 (:gens %2)) {} sgs))
  ([sgs level]
    (SGS-generators (subvec sgs (dec level)))))

;; BSGS-generators
(defn BSGS-generators 
  ([bsgs]
    (SGS-generators (:sgs bsgs)))
  ([bsgs level]
    (SGS-generators (:sgs bsgs) level)))

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
;; TODO: if not using please get rid of this
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
        (recur ss 
               (into S #{(pg/inverse s)})
              ;; check if need to extend base
               (if (pg/pointwise-stabilizer? s B)  
                 (conj B (apply min (pg/moved-points-other-than s B)))
                 B))))))

; Scherier-point->generators
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

(defn- extends-Schreier-vector?
  [sv node from]
  (and (not (contains? sv node))  ;; node not yet there
          (contains? sv from))) ;; from is there

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

;; partial-bsgs-insert-generator
(defn- partial-bsgs-insert-generator
  [p-bsgs drop-out generators]
  " insert generator at the drop-out " 
  (let [new-gens (label-generators generators (:size p-bsgs))
        old-gens (BSGS-generators p-bsgs drop-out)
        index (dec drop-out)
        ; old-sv (get-in p-bsgs [:sgs index :sv])  ;; could use this
        ;; TODO: I believe you have to update the stack in above levels
        ;; results may be incorrect nuow
        update-process-stack 
          #(update-in % [:process-stack drop-out]  ;; update process-stack
            into (Schreier-generators (into old-gens new-gens)
              (get-in % [:sgs index :sv])))
        ]
      (-> p-bsgs
        ;; TODO;; not optmizing out old gens and old orbits yet
        (update-in [:sgs index :sv]  ;; update-sv
           extend-Schreier-vector (into old-gens new-gens))
        (update-in [:sgs index :gens]  ;; update-gens
            into new-gens)
        (assoc :level drop-out)  ;; update level
        (update-in [:size] + (count new-gens))  ;; update size
        (update-process-stack)  ;; update process-stack
        )))

;; Schreier-Sims-initial-state
(defn Schreier-Sims-initial-state
  [{:keys [base generating-set]}]
  (let 
    [level (count base)
     gens (order-gens-wrt-base base generating-set)
     sv-chain (Schreier-vector-chain {:base base :sgs gens})
     gen-chain (fn [l]   ;; this is essentially S_i
                 (->> gens 
                  (filter #(>= (:lvl %) l))
                  (map :gens)
                  (into {})))
     Gstabs (map #(Schreier-generators (gen-chain %1) 
             (sv-chain %2)) (range 1 (inc level)) base)]
    {
     :sgs (into [] (map #(assoc %1 :sv (sv-chain %2)) gens base))
     :base base
     :size (count generating-set)
     :level level ; start from bottom
     :process-stack 
       (->> Gstabs (map-indexed #(vector (inc %1) 
         (into clojure.lang.PersistentQueue/EMPTY %2)))
         (into {}))
     }))

;; update-if
(defn- update-if
  [m pred x & xs]
  " update map m when pred is true " 
  (if-not pred
    m 
    (apply x m xs)))

;; Scherier-Sims-update-after-consumption
(defn- Scherier-Sims-update-after-consumption 
  [{:keys [base size] :as p-bsgs} drop-out residue]
  " update the sgs " 
  (if (empty? residue)
    p-bsgs  ;; empty residue, do nothing
    (let [extend-base? (> drop-out (count base))
          b (if-not extend-base?  ;; if we are extending the base
              (base (dec drop-out))
              (apply min (pg/moved-points-other-than residue base)))
          inv-residue (pg/inverse residue)]
      (-> p-bsgs 
          (update-if extend-base? update-in 
            [:sgs] conj {:sv (Schreier-vector-chain-stump b)
                         :lvl drop-out :gens {}})
          (update-if extend-base? update-in [:base] conj b)
          (update-if extend-base? assoc-in [:process-stack b] 
               clojure.lang.PersistentQueue/EMPTY)
          (partial-bsgs-insert-generator drop-out 
            (vector inv-residue residue))))))    

;; Schreier-Sims
(def Schreier-Sims
   (letfn 
     [(completion 
       [{:keys [process-stack level]}]
       (empty? (process-stack level)))
     (find-new 
       [{:keys [process-stack level]}]
       " the new addition in S plus the new coset rep " 
        (loop [l level]
          (if-not (empty? (process-stack l))
            (process-stack l)
            (when (> l 1) (recur (dec l))))))
     (update-stack  ;; go up
       [p-bsgs _] 
       (update-in p-bsgs [:level] dec))
     (consume-generator
       [{:keys [process-stack sgs base level] :as p-bsgs}]
       " get the residue and update " 
       (let [ g (peek (process-stack level))
              subv #(subvec % (dec level) (count base))
              [g-res drop-out] (BSGS-permutation-residue
                  {:sgs (subv sgs) :base (subv base)} g)]
         (-> p-bsgs
           (Scherier-Sims-update-after-consumption drop-out g-res)
           (update-in [:process-stack level] pop))))]

     (build-Scherier-procedure 
       completion
       find-new
       update-stack
       consume-generator)))
