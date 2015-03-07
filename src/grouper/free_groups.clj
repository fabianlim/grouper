;; free-groups
(ns grouper.free-groups
  (:require [clojure.pprint :as pp])) 

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
    (fn [e _] (subs e 0 1))
    (fn [e _] (subs e 1))))


;; follow-edge-labels-backward
(def follow-edge-labels-backward 
  (follow-edge-labels-builder 
    (fn [e l] (inverse-word (subs e (dec l) l)))
    (fn [e l] (subs e 0 (dec l)))))

;; scan-relation
(defn scan-relation
  [graph {r :rel [nf nb] :nodes}]
  " scan once forward and then once backward 
    - nf is scanned forward using rel r.
    - r is consumed to r'
    - nb is scanned backward using r'
    - again r' is consumed

    Inputs
    =====
    graph: hash-map describing the Scherier graph
    2nd arg: map of relations (rel) and nodes.
  "

  (let [[vf rr] (follow-edge-labels-forward graph nf r)
        [vb rrr] (follow-edge-labels-backward graph nb rr)]
    (hash-map :rel rrr :nodes [vf vb])))

(defn Todd-Coxeter-merge
  [x y {:keys [equivalences process-queue]}]
  " merge operator of Todd-Coxeter "
  (let [xp (min (equivalences x x) (equivalences y y))  ;; equivalences must behave like a perm map
        yp (max (equivalences x x) (equivalences y y))] ;; make xp <= yp
      (when-not (= xp yp)
        (hash-map :equivalences (equivalences yp xp) ;; make yp -> xp
                  :process-queue (conj process-queue yp)))))

        ;; (vector equivalences process-queue)  ;; nothing changed

;; (get-in{:b {:a 1}} [:b :a])
;; (get-in
;; (defn Todd-Coxeter-coincidence?
;;   [graph x y a]
;;   (every? nil? (graph [x a]) (graph [y (inverse-word a)]))
;;   )(Todd-Coxeter-coincidence? nf nb rel)
(defn Todd-Coxeter-get-unfilled-entry 
  [graph]
  " strategy for selecting the first "  
  (let [ks (->> graph (filter #(nil? (second %))) (map first))  ;; pull out the keys
        str->int #(int (inverse-word (.charAt % 0)))  ;; go from str to int, inverse is because 
        agg (fn [[x a :as one] [y b :as two]]         ;; upper-case ascii is smaller than lower-case
              (cond 
                (= x y) (vector x (min-key str->int a b)) ;; if node equals, break tie on action
                (< x y)  one
                :else two))]
    (when-not (empty? ks)
      (reduce agg ks))))

;; (pp/pprint (take 40 (Todd-Coxeter-procedure ["aaa" "bbb" "abab"] ["a"])))
;; (pp/pprint (take 100 (Todd-Coxeter-procedure ["abABB" "baBAA"] [])))

;; Todd-Coxeter-procedure  
(defn build-Todd-Coxeter-procedure 
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
    ; (let [unfilled-entry (first (filter #(nil? (second %)) graph))  ; an entry which needs filling
    (let [unfilled-entry (Todd-Coxeter-get-unfilled-entry graph)  ; an entry which needs filling
          all-scanned (empty? (:unscanned r-queues))]  
      (if-not (and all-scanned (nil? unfilled-entry))  ; queue empty and graph full
        ; (assoc prev-state :comment "done!") ; nothing let to do, return graph
        (let [[r-head & r-tail] (:unscanned r-queues)
              {rel :rel [nf nb] :nodes :as r-scanned}  ; scan the first unscanned item
                (and (not all-scanned)  ; r could be nil
                     (scan-relation graph r-head))]  
          (cond 
            (and (not all-scanned) (empty? rel))  ; consume
              (-> prev-state
                (assoc-in [:r-queues :unscanned] r-tail)
                (assoc :comment "consume"))
            (and (not all-scanned) (= (count rel) 1))  ; possibly a deduction
              (if (every? nil? (vector (graph [nf rel]) (graph [nb (inverse-word rel)])))  
                ;; this tests of coincidence
                (-> prev-state  ;; if not a coincidences, then deduce for sure
                    (assoc-in [:graph [nf rel]] nb)
                    (assoc-in [:graph [nb (inverse-word rel)]] nf)
                    (assoc-in [:r-queues :unscanned] r-tail)
                    (assoc :comment "deduce"))
                (-> prev-state 
                  (assoc :coset-meta  ;; update coset-meta (merge nf and nb)
                    (Todd-Coxeter-merge nf nb (:equivalences :process-queue coset-meta)))
                  (assoc-in [:r-queues :unscanned] r-tail)
                  (assoc :comment "coincidence")))
              (and (not all-scanned) (not (nil? r-tail)))  ;; keep on scanning
                (-> prev-state 
                    (assoc-in [:r-queues :unscanned] r-tail)
                    (assoc-in [:r-queues :scanned] (cons r-scanned (:scanned r-queues)))
                    (assoc :comment "keep-scanning"))
            :else  ; generate a new node
              (let [[node act :as entry] unfilled-entry 
                    new-label (:coset-next-label coset-meta)
                    new-empty-row (reduce #(assoc %1 [new-label %2] nil [new-label (.toUpperCase %2)] nil) 
                      {} generators)  ; generators bound by clojure
                    graph-with-extra-row (into graph new-empty-row)
                    ;; relations bound by clojure
                    new-relations (map #(hash-map :rel % :nodes [new-label new-label]) relations)]
                (-> prev-state
                    (assoc :graph graph-with-extra-row)
                    (assoc-in [:graph entry] new-label)
                    (assoc-in [:graph [new-label (inverse-word act)]] node)
                    (assoc-in [:coset-meta :coset-next-label] (inc new-label))
                    (assoc-in [:r-queues :unscanned] 
                              (reduce concat [(:scanned r-queues) 
                                              (if r-scanned (vector r-scanned) (vector))
                                              new-relations]))
                    (assoc-in [:r-queues :scanned] [])
                    (assoc :comment "new-node")))))))))

(defn Todd-Coxeter-procedure
  [relations subgroup-relations]
    " the Todd-Coxeter procedure. 
      - given a presentation for a group G and subgroup H
      - finds a Caley graph of the group action on cosets of H"
    (let [all-relations (into subgroup-relations relations)
          generators (->> all-relations 
                          (reduce #(into %1 %2) #{})
                          (map str))
          initial-graph (reduce #(assoc %1 [1 %2] nil [1 (inverse-word %2)] nil) {} generators)
          initial-queue (map #(hash-map :rel % :nodes [1 1]) all-relations)
          initial-state {:graph initial-graph 
                         :coset-meta {:coset-next-label 2 :equivalences {1 1} :process-queue []}
                         :r-queues {:unscanned initial-queue}
                         :comment "initial"} 
          step (build-Todd-Coxeter-procedure generators relations subgroup-relations)]
        (take-while (comp not nil?) (iterate step initial-state)))) 
