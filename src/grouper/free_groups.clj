;; free-groups
(ns grouper.free-groups)

; simplify method for free-groups
(defn simplify-relations [word & [relations]]
  " simplifies words in the free group

    Inputs
    =====
    word: a string representative of a word
    relations: a map which specifies what needs to be replaced
      e.g. [k v] => [from to]
  "
  (let [rel 
          ;; these are the relations for neutralizing an element with its inverse
          (assoc (or relations {}) 
               #"(\p{Lu})(?=\p{Ll})(?i)\1" "" ;; relation Aa=1
               #"(\p{Ll})(?=\p{Lu})(?i)\1" "") ;; relation aA=1
        list-of-relation-reps 
          ;; convert the map of relations (rel) to a list of 
          ;; replacement functions
          (map (fn [[this to]] #(clojure.string/replace % this to)) (seq rel))
       simplify-word-using-relations
          ;; simplify word using the relations
          (fn [w]
            ((apply comp list-of-relation-reps) w))
        ]
    (loop [p word] 
      (let [x (simplify-word-using-relations p)]
        (if (= x p)
          p
          (recur x))))))

;; multi-method inverse-word
(defmulti inverse-word class)

(defmethod inverse-word java.lang.Character [x]
  " returns word that delivers the inverse action "
  (if (Character/isUpperCase x)
    (Character/toLowerCase x)
    (Character/toUpperCase x)))

(defmethod inverse-word java.lang.String [word]
  " returns word that delivers the inverse action "
  (letfn [(map-toggle-case [x]
            ;; create a mapped version of toggle-case so we can use -> below
           (map #(inverse-word %) x))]
    (-> word clojure.string/reverse map-toggle-case clojure.string/join)))

(defn follow-edge-labels-builder
  [get-next-edge get-rest-edge-seq]
    " uses the graph data structure and follows the edge-label sequence
      starting from node. 
      
      Inputs
      =====
      graph: a map with keys ([from-node edge-label]) and values (to-node)
      edge-label-sequence: clojure string of edge labels
      node: the starting node
    
      =====
      - Consumes the edge-label-sequence as it follows the graph.
      - Returns un-consumed part if consumption is not possible given the graph
    "
  (fn [graph node edge-label-sequence]
    (loop [e-seq edge-label-sequence
           x node
           l (count edge-label-sequence)]
        (let [s (and (not-empty e-seq) (get-next-edge e-seq l))  ; get next e label
              xn (graph [x s])]  ; get next node
          (if xn
            (recur (get-rest-edge-seq e-seq l) xn (dec l))
            (vector x e-seq))))))

;; follow-edge-labels-forward
(def follow-edge-labels-forward 
  (follow-edge-labels-builder 
    (fn [e _] 
      (when (> (count e) 1)  ;; only consume if relation is > length 1
        (subs e 0 1)))
    (fn [e _] (subs e 1))))


;; follow-edge-labels-backward
(def follow-edge-labels-backward 
  (follow-edge-labels-builder 
    (fn [e l] 
      (when (> (count e) 1)  ;; only consume if relation is > length 1
        (inverse-word (subs e (dec l) l))))
    (fn [e l] (subs e 0 (dec l)))))


;; scan-relation
(defn scan-relation
  [graph {r :rel [nf nb] :nodes :as relation}]
  " scan once forward and then once backward 
    - nf is scanned forward using rel r.
    - r is consumed to r'
    - nb is scanned backward using r'
    - again r' is consumed
    - stop when relation is of length one

    Inputs
    =====
    * graph: hash-map describing the Scherier graph
    * rel: string describing the word
    * nf: forward node
    * nb : backward node
  "

  (let [[vf rr] (follow-edge-labels-forward graph nf r)
        [vb rrr] (follow-edge-labels-backward graph nb rr)]
    (assoc relation :rel rrr :nodes [vf vb])))

;; Todd-Coxeter-merge
(defn Todd-Coxeter-merge
  [x y {:keys [equivalences process-queue] :as coset-meta}]
  " merge procedure of the Todd-Coxeter algorithm 

    Inputs
    ======
    * x: node to be processed
    * y: node to be processed
    * equivalences: map describing node equivalences
      for sparsity sake, keys not contained in the map asumed to map to themselves
    * process-queue: clojure.lang.PersistentQueue queues nodes needing to be processed
  "
  (let [xp (min (equivalences x x) (equivalences y y))  ;; equivalences must behave like a perm map
        yp (max (equivalences x x) (equivalences y y))] ;; make xp <= yp
      (if-not (= xp yp)
        (into coset-meta
          (hash-map :equivalences (assoc equivalences yp xp) ;; make yp -> xp
                    :process-queue (conj process-queue yp)))
        coset-meta)))

;; Todd-Coxeter-process-coincidence
(defn Todd-Coxeter-process-coincidence
  [{:keys [coset-meta graph]}]
  " process coincidences in to Todd-Coxeter algorithm

    Inputs
    ======
    * coset-meta: map containing information on the cosets
      (contains process-queue, equivalences and coset-next-label)   
    * graph: Scherier-graph
  "
  (let [{:keys [process-queue equivalences coset-next-label]} coset-meta
        v (first process-queue)
        vp (equivalences v v)
        entry-seq (filter #(= (first (first %)) v) graph)  ;; find all u where u = v^s for some s
        ]
    (when-not (nil? v)
      (loop [[entry & entrys] entry-seq
             G graph
             cm (assoc coset-meta :process-queue (pop process-queue))  ; coset-meta with tail of process queue
             ]  
          (if (nil? entry)
            {:graph G :coset-meta cm :comment (str "process " v)}
            (let [[[_ s] u] entry
                  up (equivalences u u) 
                   S (inverse-word s)
                   g (dissoc G [u S] [v s]) ;; delete the edge (v^s = u and u^S = v) 
                   w1 (g [vp s])]  ; check if w exists where vp^s = w
              (if (not (nil? w1))  ;; does not exist any w where vp^s = w
                  (recur entrys g (Todd-Coxeter-merge w1 up cm))  ;; merge w = vp^s with up
                  ; or else vp^s and up^S are not defined
                  (recur entrys (assoc g [vp s] up [up S] vp) cm)
          )))))))

;; Todd-Coxeter-get-unfilled-entry
;; looks at the relation queue and picks the relation with 
;; the smallest length, i.e., picks the relation which is going to complete
;; scanning soon
(defn Todd-Coxeter-get-unfilled-entry 
  [relations]
  " strategy for selecting the first. 
    Chooses the node to be added, by picking in form one of the relations that
    are close to completing

    Inputs
    =====
    * relations: vector of relations 
  "  
  (let [{[n _] :nodes r :rel} (apply min-key (comp count :rel) relations)]
      (vector n (subs r 0 1))))


;; Todd-Coxeter-procedure  
(defn Todd-Coxeter-builder-state-machine
  [generators relations subgroup-relations]
  " build the Todd-Coxeter procedure. 

    Inputs 
    ======
    generators: vector of generators, e.g. ['a', 'b']
    relations: vector of relations enforced by the presentation, 
               e.g. ['aaa', 'bbb', 'abab']
    subgroup-relations: vector of subgroup relations enforced by
              the presentation, e.g. ['a']

    Readings 
    =======
    Online notes:
    Ken Brown
    Cornell University, Math 7350
    http://www.math.cornell.edu/~kbrown/7350/toddcox.pdf
  "

  (fn [{:keys [graph coset-meta r-queues] :as prev-state}]
    " the state machine of Todd-Coxeter " 
    (let [unscanned (filter #(not (contains? (:equivalences coset-meta)  ;; filter out dead nodes
                                          (:row %))) (:unscanned r-queues))] 
     (if (and (empty? unscanned) (not (empty? (:scanned r-queues))))
        (-> prev-state (assoc-in [:r-queues :unscanned] (:scanned r-queues))  ;; check for exhausted unscanned
            (assoc-in [:r-queues :scanned] (list)))
        (when-not (and (empty? unscanned) (empty? (:process-queue coset-meta))) 
          ;; otherwise proceed only if
          ;; there is more to scan, or it there are nodes to process
          (let [ r-tail (rest unscanned)  ;; tail of unscanned
                {rel :rel [nf nb] :nodes :as r-scanned}  ; scan the first unscanned item
                  (and (not (empty? unscanned))  ; head could be nil
                       (scan-relation graph (first unscanned)))]  
            (cond 
                (not (empty? (:process-queue coset-meta)))  ;; process coincidences
                  (into prev-state (Todd-Coxeter-process-coincidence 
                                     {:coset-meta coset-meta :graph graph}))
                (= (count rel) 1)  ; possibly a deduction
                  (cond 
                    (and (= (graph [nf rel]) nb) (= (graph [nb (inverse-word rel)]) nf))  ;; consumption
                      (-> prev-state
                        (assoc-in [:r-queues :unscanned] r-tail)
                        (assoc :comment "consume"))
                    (every? nil? (vector (graph [nf rel]) (graph [nb (inverse-word rel)])))  
                      (-> prev-state  ;; a deduction in this case
                          (assoc-in [:graph [nf rel]] nb)
                          (assoc-in [:graph [nb (inverse-word rel)]] nf)
                          (assoc-in [:r-queues :unscanned] r-tail)  ; TODO : dont need to refresh scanned?
                          ; (assoc-in [:r-queues :unscanned] (concat r-tail (:scanned r-queues)))
                          ; (assoc-in [:r-queues :scanned] [])
                          (assoc :comment 
                                 (str "deduce [" nf "," rel "]=" nb)))
                    :else   ;; must otherwise be an coincidence
                      (let [x (if (graph [nf rel]) nb nf)  ; the rhs
                            y (or (graph [nf rel]) (graph [nb (inverse-word rel)]))] ; the value
                        (-> prev-state 
                          (assoc :coset-meta  ;; update coset-meta (merge nf and nb)
                            (Todd-Coxeter-merge x y (:equivalences :process-queue coset-meta)))
                          (assoc-in [:r-queues :unscanned] r-tail)
                          (assoc :comment (str "coincidence " x "=" y)))))
                (not (empty? r-tail))  ;; keep on scannng
                  (-> prev-state 
                      (assoc-in [:r-queues :unscanned] r-tail)
                      (assoc-in [:r-queues :scanned] (cons r-scanned (:scanned r-queues)))
                      (assoc :comment "keep-scanning"))
                :else  ; generate a new node
                  (let [[node act :as entry] 
                          (Todd-Coxeter-get-unfilled-entry (cons r-scanned (:scanned r-queues)))
                        new-label (:coset-next-label coset-meta)
                        ;; relations bound by clojure
                        new-relations (map #(hash-map :rel % 
                                                      :nodes [new-label new-label] 
                                                      :row new-label) relations)]
                    (-> prev-state
                        (assoc-in [:graph entry] new-label)
                        (assoc-in [:graph [new-label (inverse-word act)]] node)
                        (assoc-in [:coset-meta :coset-next-label] (inc new-label))
                        (assoc-in [:r-queues :unscanned] 
                                  (reduce concat [(:scanned r-queues) 
                                      (cons r-scanned new-relations)]))
                        (assoc-in [:r-queues :scanned] [])
                        (assoc :comment 
                               (str "new-node [" node "," act "]=" new-label)))))))))))


(defn Todd-Coxeter-builder-initial-state
  [generators all-relations]
    " builds the initial state to iterate with the Todd-Coxeter algorithm. 

      Inputs
      ======
      * generators: 
      * all-relations: vector of all relations (both coset and subgroup)
    "
    (let [initial-graph (reduce #(assoc %1 [1 %2] nil [1 (inverse-word %2)] nil) {} generators)
          ;; initial-queue (map #(hash-map :rel % :nodes [1 1]) all-relations)]
          initial-queue (map #(hash-map :rel % :nodes [1 1] :row 1) all-relations)]
        {:graph initial-graph :coset-meta 
           {:coset-next-label 2 
            :equivalences (hash-map) 
            :process-queue (clojure.lang.PersistentQueue/EMPTY)}
             :r-queues {:unscanned initial-queue}
             :comment "initial"}))

(defn Todd-Coxeter-procedure
  [relations subgroup-relations]
    " the Todd-Coxeter procedure. 
      - given a presentation for a group G and subgroup H
      - finds a Caley graph of the group action on cosets of H"
    (let [all-relations (into subgroup-relations relations)
          generators (->> all-relations (reduce #(into %1 %2) #{}) (map str))
          sm (Todd-Coxeter-builder-state-machine generators relations subgroup-relations)
          init (Todd-Coxeter-builder-initial-state generators all-relations)]
      (take-while (comp not nil?) (iterate sm init))))

