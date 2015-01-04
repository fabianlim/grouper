(ns grouper.number-groups
  (:require [grouper.core-math-structures :as co]))

;; inline implementation of *Integers* over N
;; this is isomorphic *Quotient* Z / (N Z)
(defrecord ZmodN [N]
  ;;  Additive Group of Z/NZ
  ;;  ==========================

  ;;  The multiplicative action should be implemented somewhere else
  ;;  (since it is not necessarily a group) 
  
  co/Monoid 
  (co/action [this] 
    (defn act 
      " Action implemented over the *Integer* *Domain* using the
        mod operation.  "
      ([x]
       ;; TODO : good form to do this?
       ;; single-arity act viewed as a projection 
       ;; " single-arity simply takes mod"
       (clojure.core/mod x (:N this)))
      ([x y]
        (act (clojure.core/+ x y)))))

  (co/identity-element-impl [this]
    ;; additive identity is implemented by the *Integer* 0
    0)

  co/Category 
  (co/morphism-factory [this]
    ;; build the morphism from using the action
    (fn [value] 
      (co/->MorphismFromAction value (co/action this)))))
