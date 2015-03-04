;; free-groups
(ns grouper.free-groups) 

; simplify method for free-groups
(defn simplify-relations [word & [relations]]
  " simplifies words to belong in the free group
    - cancels out neighboring elements and their inverse
    - relations is a map that specifies what needs to be replaed 
      - [k v] => [from to]
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

;; (simplify-relations "BbaAaBB")

(defn inverse-word [word]
  " returns word that delivers the inverse action "
  (letfn [(toggle-case [x]
            ;; toggle the case of a character
            (if (Character/isUpperCase x)
              (clojure.string/lower-case x)
              (clojure.string/upper-case x)))
          (map-toggle-case [x]
            ;; create a mapped version of toggle-case so we can use -> below
           (map #(toggle-case %) x))]
    (-> word clojure.string/reverse map-toggle-case clojure.string/join)))

;; (inverse-word "BBaa")

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
      Consumes the edge-label-sequence as it follows the graph.
      Returns un-consumed part if consumption is not possible given the graph
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

(def follow-edge-labels-forward 
  (follow-edge-labels-builder 
    (fn [e _] (subs e 0 1))
    (fn [e _] (subs e 1))))

(def follow-edge-labels-backward 
  (follow-edge-labels-builder 
    (fn [e l] (inverse-word (subs e (dec l) l)))
    (fn [e l] (subs e 0 (dec l)))))

;; (follow-edge-labels coset-table "a" 1)
;; (follow-edge-labels coset-table (inverse-word "a") 1)
;; (follow-edge-labels {[1 "a"] 2} "ab" 1)
;; (follow-edge-labels-forward {[1 "a"] 2 [1 "B"] 3} 1 "ab")
;; (follow-edge-labels-backward {[1 "a"] 2 [1 "B"] 3} 1 "ab")

(defn scan-relation
  [graph {r :rels [nf nb] :nodes}]
  " scan forward and backward "
  (let [[vf rr] (follow-edge-labels-forward graph nf r)
        [vb rrr] (follow-edge-labels-backward graph nb rr)]
    (hash-map :rels rrr :nodes [vf vb])))

;; (let [graph
;;     {[1 "B"] 2,
;;        [1 "A"] 1,
;;        [1 "a"] 1,
;;        [1 "b"] nil,
;;        [2 "B"] nil,
;;        [2 "b"] 1,
;;        [2 "A"] nil,
;;        [2 "a"] nil}]
;;  (do 
;;   (follow-edge-labels-forward graph 2 "bbb")
;;   (scan-relation graph {:rels "bbb" :nodes [2 2]})))

;; (scan-relation coset-table {:rels "a" :nodes [1 1]})
;; (scan-relation {[2 "a"] 3} {:rels "a" :nodes [1 1]})

;; for testing
(let [step (Todd-Coxeter-procedure-builder ["a" "b"] ["aaa" "bbb" "abab"] ["a"])
      initial-state {:graph {[1 "a"] nil [1 "A"] nil [1 "b"] nil [1 "B"] nil}
                     :coset-meta {:coset-next-label 2 :coset-equivalences {1 1}}
                     :r-queues {:unscanned (vector {:rels "a" :nodes [1 1]} 
                                        {:rels "aaa" :nodes [1 1]} 
                                        {:rels "bbb" :nodes [1 1]}
                                        {:rels "abab" :nodes [1 1]})}
                     :comment "initial"}]
      (clojure.pprint/pprint (take 30 (iterate step initial-state))))

  
(defn Todd-Coxeter-procedure-builder 
  [generators relations subgroup-relations]
  (fn [{:keys [graph coset-meta r-queues] :as prev-state}]
    " the state machine of Todd-Coxeter " 
    " Todd-Coxeter procedure
      ======================
      - given a presentation for a group G and subgroup H
      - finds a Caley graph of the group action on cosets of H
    "
    (let [unfilled-entry (first (filter #(nil? (second %)) graph))  ; an entry which needs filling
          all-scanned (empty? (:unscanned r-queues))]  
      (if (and all-scanned (nil? unfilled-entry))  ; queue empty and graph full
        (assoc prev-state :comment "done!") ; nothing let to do, return graph
        (let [[r-head & r-tail] (:unscanned r-queues)
              {rels :rels [nf nb] :nodes :as r-scanned}  ; scan the first unscanned item
                (and (not all-scanned)  ; r could be nil
                     (scan-relation graph r-head))]  
          (cond 
            (and (not all-scanned) (empty? rels))  ; consume
              (-> prev-state
                (assoc-in [:r-queues :unscanned] r-tail)
                (assoc :comment "consume"))
            (and (not all-scanned) (= (count rels) 1))  ; deduce
              (-> prev-state 
                  (assoc-in [:graph [nf rels]] nb)
                  (assoc-in [:graph [nb (inverse-word rels)]] nf)
                  (assoc-in [:r-queues :unscanned] r-tail)
                  (assoc :comment "deduce"))
              (and (not all-scanned) (not (nil? r-tail)))  ;; keep on scanning
                (-> prev-state 
                    (assoc-in [:r-queues :unscanned] r-tail)
                    (assoc-in [:r-queues :scanned] (cons r-scanned (:scanned r-queues)))
                    (assoc :comment "keep-scanning"))
            :else  ; generate a new node
              (let [[[node act :as entry] _] unfilled-entry 
                    new-label (:coset-next-label coset-meta)
                    new-empty-row (reduce #(assoc %1 [new-label %2] nil [new-label (.toUpperCase %2)] nil) 
                      {} generators)  ; generators bound by clojure
                    graph-with-extra-row (into graph new-empty-row)
                    ;; relations bound by clojure
                    new-relations (map #(hash-map :rels % :nodes [new-label new-label]) relations)]
                (-> prev-state
                    (assoc :graph graph-with-extra-row)
                    (assoc-in [:graph entry] new-label)
                    (assoc-in [:graph [new-label (inverse-word act)]] node)
                    (assoc-in [:coset-meta :coset-next-label] (inc new-label))
                    (assoc-in [:r-queues :unscanned] 
                              (reduce concat [(:scanned r-queues) 
                                              (if r-scanned
                                                (vector r-scanned)
                                                (vector))
                                              new-relations]))
                    (assoc-in [:r-queues :scanned] [])
                    (assoc :comment "new-node")))))))))

