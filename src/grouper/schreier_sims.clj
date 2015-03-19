;; schreier-sims
;; TODO: implement for permutation groups first
(ns grouper.schreier-sims
  (:require [grouper.permutation-groups :as pg]))

(def Sym4 {:base (vector 1 2 3)
           :sgs (vector 
                 (pg/cyc-notation [1 2])
                 (pg/cyc-notation [2 3])
                 (pg/cyc-notation [3 4]))})

;; compute-schreier-vector
;; actually a BFS 
(defn compute-schreier-vector
  [root generating-set]
  (loop [tree {root {:from nil :gen nil}}
         queue (conj clojure.lang.PersistentQueue/EMPTY root)]
      (if (empty? queue)
        tree
        (let [x (peek queue)  ;; current item
          new-items (->> generating-set
            (map-indexed (fn [i gen] (vector (gen x) i)))  ;; map [item gen-index] pairs
            (filter #(first %)) ;; filter out items that are nil 
            (filter #(not (contains? tree (first %)))))]  ;; filter items not in tree
          (recur (reduce #(assoc %1 (first %2) 
             {:from x :gen (second %2)}) tree new-items) 
             (reduce conj (pop queue) (map first new-items)))))))
