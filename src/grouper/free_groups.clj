;; free-groups
(ns grouper.free-groups)

; simplify method for free-groups
(defn simplify-relations [word & [relations]]
  " simplifies words to belong in the free group
    - cancels out neighboring elements and their inverse
    - relations is a map that specifies what needs to be replaed 
      - [k v] => [from to]
  "
  (let [rel (assoc (or relations {}) ;; we must always have these relations
                   #"(\p{Lu})(?=\p{Ll})(?i)\1" "" ;; relation Aa=1
                   #"(\p{Ll})(?=\p{Lu})(?i)\1" "" ;; relation aA=1
                   )]
    (letfn [(list-of-relation-reps []
              ;; take relations in a map form and produce a list of
              ;; replacement functions
              (map (fn [[this to]] #(clojure.string/replace % this to)) (seq rel)))
            (helper-replace [w]
              ;; simplify w with all those relations
              ((apply comp (list-of-relation-reps)) w))
            (helper [p]
              ;; main helper function
              (let [x (helper-replace p)]
                (if (= x p)
                  p
                  (recur x))))]
      (helper word))))

;; (simplify-relations "BbaAaBB")
;; (simplify-relations "baAB")

;; Todd-Coxeter procedure
(defn Todd-Coxeter-procedure 
  [G]
  " Todd-Coxeter procedure
    ======================
    - given a presentation for a group G and subgroup H
    - finds a Caley graph of the group action on cosets of H
  "
  (let [{G-rel :relations} G]
    (println G-rel)
    ))

(Todd-Coxeter-procedure {:relations "this"})
