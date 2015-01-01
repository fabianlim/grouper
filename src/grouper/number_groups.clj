(ns grouper.number-groups
  (:require [grouper.core-objects :as co]))

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
    (let [monoid-action (co/action this)]
      " bindings
        ========

        * monoid-action binds to the action "

      (defn morphism [value]
        " ZmodN (Element of)
          =================
        
          Implemented (parameterized) as an *Integer*. 
          This follows from the implementation of the monoid action "

        (let [r ;; build object and bind it
              (co/boilerplate-morphism morphism value monoid-action)]
          (extend-protocol co/Morphism
            r 
            (inverse [_] ;; construct the inverse element
              ;; bypass the previledged argument and just
              ;; use value
              (morphism (monoid-action (- value))))
            )
 
          (defmethod print-method (type r) [this ^java.io.Writer w] 
            ;; further define a print method
            (.write w (str (co/morphism-impl this))) )

          ;; finally returns the object r
          r)))))
