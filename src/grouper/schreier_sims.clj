;; schreier-sims
;; TODO: implement for permutation groups first
(ns grouper.schreier-sims
  (:require [grouper.permutation-groups :as pg]
            [grouper.free-groups :as fg]
            [grouper.core-group-functions :as co]))


;; extend-Schreier-vector
(defn extend-Schreier-vector
  [init-tree generating-set]
  " extend Schreier tree from init-tree " 
  (letfn [
    (generate-new-items [x result]
      (->> generating-set   ;; generate-new-items
        (map (fn [[n gen]] (vector (gen x) n)))  ; image-label pairs
        (filter #(first %)) ;; filter out nil images
        (filter #(not (contains? result (first %))))))
    (update-result [x result new-items]
        (reduce #(assoc %1 (first %2) 
           {:from x :gen (second %2)}) result new-items))
    (populate-queue [new-items] (map first new-items))]
  ;; eval function
  ((co/bfs-builder generate-new-items update-result populate-queue)
    init-tree (keys init-tree))))


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

;; Schreier-vector-reduce
(defn- Schreier-vector-reduce
  [reducer init sv point]
  " a generic reducer function to compute elements given a walk 
    of generator labels from point->root "
  (->> point
       (Schreier-vector-point->root sv)
       (map :gen)
       (reduce reducer init)))

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
  (Schreier-vector-reduce 
    #(pg/compose %1 (generators %2)) {} sv point))

;; TODO: consider making a builder function
;; BSGS-perm-residue
(defn BSGS-permutation-residue
  ([{:keys [sgs base]} perm compose inverse coset-leader image]
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
      (let [x (image g b)  
            gens (SGS-generators S)
            sv (or (get-in S [0 :sv])
                   (Schreier-vector b gens))
            cl (coset-leader gens sv x)]
        (if-not (contains? sv x)
          [g lvl]
          (recur (compose g (inverse cl)) bs
            ; (filter #(> (:lvl %) lvl) S)    
            (subvec S 1)
            (inc lvl)))))))
  ([bsgs perm]
   (BSGS-permutation-residue bsgs perm 
      pg/compose pg/inverse Schreier-vector-coset-leader
      #(%1 %2 %2))))

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

;; partial-BSGS
(defn partial-BSGS
  " compute a partial-BSGS

    a partial-BSGS satisfies the following:
    * contains a generating set
    * closed under inversion
    * all permutations must not fix (pointwise?) every element in 
      the base

    Inputs
    ======
    base-points: vector of points to include in the base
    SGS-perms: map of permutations to include in the SGS
    generating-set: map of perms that the partial-BSGS will contain
    * SGS-perms and generating-set should have string keys that 
      name them. 
    * Make sure their names do not collide.
  " 
  ([base-points SGS-perms generating-set select-base-point]
    (loop [l 1
           B base-points
           S (-> (clojure.set/map-invert 
                 (into SGS-perms generating-set))
               (dissoc {}))  ;; remove identity perms (if any)
           sgs (vector)]
      (if (> l (count B))
        {:base B :sgs sgs}
        (let [sub-base (if (> l (count B)) B (subvec B 0 l))
              stabilizers (filter 
                            #(pg/pointwise-stabilizer? % sub-base)
                           (keys S))
              gens (apply dissoc S stabilizers)
              gens-inv (->> gens
                         (map #(vector (pg/inverse (first %))
                               (fg/inverse-word (second %))))
                         (filter #(not (contains? gens (first %)))))
              other-points #(pg/moved-points-other-than % sub-base)
                b (select-base-point (reduce 
                         #(into %1 (other-points %2)) #{} (keys S)))]
          (println "S" S)
          (println "base" B)
          (println "l" l)
          (println "other-points" other-points)
          (if (and (empty? gens) (not (empty? S)))
            (recur l (conj B b) S sgs)
            (recur (inc l) B 
                   (apply dissoc S (map first gens))
                   (conj sgs {:lvl l 
                              :gens (clojure.set/map-invert 
                                  (into gens gens-inv))})))))))
    ([base-points SGS-perms generating-set]
     " version that selects base by min " 
     (partial-BSGS base-points 
                   SGS-perms generating-set #(apply min %))))

; compose helper
(defn- compose-tagged
  [[a g] [b h]]
  " compose helper for perms tagged with word " 
  (vector (fg/simplify-relations (str a b)) (pg/compose g h)))

; inverse helper
(defn- inverse-tagged
  [[a g]]
  " inverse helper for perms tagged with word " 
  (vector (fg/inverse-word a) (pg/inverse g)))

; coset-leader helper
(defn- coset-leader-tagged
  [generators sv point] 
  " coset-leader helper that works with compose helper above " 
  (Schreier-vector-reduce
    #(compose-tagged %1 [%2 (generators %2)]) ["" {}] sv point))

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

    Outputs 
    =======
    returns a sequence of [gen label] 
  " 

  (let [images (map #(% point point) (vals generators))
        cl ;; note: this gives element mapping x->base
          #(coset-leader-tagged generators sv %)
        cl-point (cl point)]
    (filter (comp not empty? second) ;; take out identity elements
      (map (fn [s image-s] 
            (-> (compose-tagged cl-point s)
                (compose-tagged (inverse-tagged (cl image-s)))))
           generators images))))

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
  [p-bsgs drop-out new-gens]
  " insert generator at the drop-out " 
  (let [old-gens (BSGS-generators p-bsgs drop-out)
        index (dec drop-out)
        ; old-sv (get-in p-bsgs [:sgs index :sv])  ;; could use this
        ;; TODO: I believe you have to update the stack in above levels
        ;; results may be incorrect nuow
        update-process-stack 
          (fn [m] 
            (loop [x drop-out
                   result m]
              (if (< x drop-out); (= x 0)
                result 
                (recur (dec x)
                  (let [S (Schreier-generators (into old-gens new-gens)
                            (get-in m [:sgs (dec x) :sv]))]
                    (update-in result [:process-stack x] into S))))))
        update-sv-chain
          (fn [m]
              (loop [x index
                     r m]
                (if (= x 0)
                  r 
                  (let [G (BSGS-generators m (inc x))]
                  (recur (dec x)
                    (-> r
                      (update-in [:sgs x :sv]  ;; update-sv
                         extend-Schreier-vector G)
                      (assoc-in [:sgs x :gens] G)))))))
          ]
      (-> p-bsgs
        ;; TODO;; not optmizing out old gens and old orbits yet
        (update-in [:sgs index :sv]  ;; update-sv
           extend-Schreier-vector (into old-gens new-gens))
        (update-in [:sgs index :gens]  ;; update-gens
            into new-gens)
        (update-sv-chain)
        (assoc :level drop-out)  ;; update level
        ; (update-in [:size] + (count new-gens))  ;; update size
        (update-process-stack)  ;; update process-stack
        )))

;; Schreier-Sims-initial-state
(defn Schreier-Sims-initial-state
  ([{:keys [base sgs] :as p-bsgs} choose-point]
  " gens is a vector of {:lvl , :gens {'a' ..}} " 
  (let 
    [level (count base)
     sv-chain (Schreier-vector-chain p-bsgs)
     gen-chain (fn [l]   ;; this is essentially S_i
                 (->> sgs
                  (filter #(>= (:lvl %) l))
                  (map :gens)
                  (into {})))
     Gstabs (map #(Schreier-generators (gen-chain %1) 
             (sv-chain %2)) (range 1 (inc level)) base)]
    {
     :sgs (into [] (map #(assoc %1 :sv (sv-chain %2)) sgs base))
     :base base
     :level level ; start from bottom
     :process-stack 
       (->> Gstabs (map-indexed #(vector (inc %1) 
         (into clojure.lang.PersistentQueue/EMPTY %2)))
         (into {}))
     :choose choose-point}))
  ([p-bsgs]
   (Schreier-Sims-initial-state p-bsgs #(apply min %))))

;; update-if
(defn- update-if
  [m pred x & xs]
  " update map m when pred is true " 
  (if-not pred
    m 
    (apply x m xs)))

;; Schreier-vector-chain-stump
(defn- Schreier-vector-chain-stump
  [b]
  (hash-map b {:gen nil :from nil}))

;;
(defn- pair-with-inverse 
  " returns a hash map of g and g^-1 (if exists) " 
  [l g]
  (let [g-inv (pg/inverse g)
        l-inv (fg/inverse-word l)]
    (if (= g g-inv)
      {l g}
      {l g l-inv g-inv})))

;; Scherier-Sims-update-after-consumption
(defn- Scherier-Sims-update-after-consumption 
  [{:keys [base size choose] :as p-bsgs} drop-out residue]
  " update the sgs " 
  (if (empty? (second residue))
    p-bsgs  ;; empty residue, do nothing
    (let [extend-base? (> drop-out (count base))
          b (if-not extend-base?  ;; if we are extending the base
              (base (dec drop-out))
              (choose 
                (pg/moved-points-other-than (second residue) base))) ]
      (-> p-bsgs 
          (update-if extend-base? update-in 
            [:sgs] conj {:sv (Schreier-vector-chain-stump b)
                         :lvl drop-out :gens {}})
          (update-if extend-base? update-in [:base] conj b)
          (update-if extend-base? assoc-in [:process-stack drop-out] 
               clojure.lang.PersistentQueue/EMPTY)
          (partial-bsgs-insert-generator drop-out 
             (apply pair-with-inverse residue))))))

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
              [g-res drop-out] 
                (BSGS-permutation-residue
                  {:sgs (subv sgs) :base (subv base)} g
                  compose-tagged inverse-tagged
                  coset-leader-tagged #(get-in %1 [1 %2] %2))]
         (-> p-bsgs
           (Scherier-Sims-update-after-consumption 
             drop-out g-res)
           (update-in [:process-stack level] pop))))]

     (build-Scherier-procedure 
       completion
       find-new
       update-stack
       consume-generator)))
