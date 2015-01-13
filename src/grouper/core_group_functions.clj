(ns grouper.core-group-functions)

; TODO : how to detect elements with infinite order?
(defn build-order-function
  " return the order of elem "
  [action]
  (fn [elem]
    (letfn 
      [(helper [acc ord]
        (if 
          (= acc elem)
          (dec ord) ; need to reduce by 1
          (recur (action acc elem) (inc ord))))]
    (helper (action elem elem) 2))))

;; (defn cartesian-product [G H]
;;   co/Monoid
;;   (co/action [this]
;;   )
;;   (let [g 
;;         
;;         ])
;;   [x y]
;;   )

;; TODO : extend something like this for direct product action?
;; recall elements are Morphisms on a particular Domain, so this
;; takes it into a product of morphisms on a product of Domains
;; (map co/identity-element [(->ZmodN 5) (->ZmodN 3)])
;; 
;; (let [a (map co/element [(->ZmodN 5) (->ZmodN 3)] [2 4])
;;       b (map co/element [(->ZmodN 5) (->ZmodN 3)] [1 2])]
;;   (map co/compose a b))
