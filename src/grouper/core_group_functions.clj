(ns grouper.core-group-functions)

; TODO : how to detect elements with infinite order?
(defn build-order-function
  " return the order of elem "
  [action]
  (fn [elem]
    (loop [acc (action elem elem) ord 2]
      (if (= acc elem)
        (dec ord) ; need to reduce by 1
        (recur (action acc elem) (inc ord))))))

(defn build-product-action
  " build a product action given a list of them "
  [actions]
  (fn [this other] 
    (map #(%1 %2 %3) actions this other)))

; TODO this macro is possibly a bad idea
;; unpack-args macro 
;; (defmacro unpack-args [f arglist]
;;   " unpack args to function f"
;;   `(~f ~@arglist))


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
