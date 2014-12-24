(ns grouper.number-groups
  (:require [grouper.core-objects :as co]))

;; inline implementation of *Integers* over N
;; this is isomorphic *Quotient* Z / (N Z)
;; TODO : make a generic Quotient Interface ? 
(defrecord ZmodN [N]
  co/Monoid 
  (co/action [this] 
    (defn act 
      " Action implemented over the *Integer* *Domain* using the
        mod operation.
      "
      ([x]
       ;; TODO : good form to do this?
       " single-arity simply takes mod"
       (clojure.core/mod x (:N this)))
      ([x y]
        (act (clojure.core/+ x y)))))

  (co/identity-element-impl [this]
    0 ;; additive identity is implemented by the *Integer* 0
  )

  co/Category 
  (co/morphism-factory [this]
    (let [monoid-action (co/action this)]
      " 
        bindings
        ========

        * monoid-action binds to the action "

      (defn morphism [value]
        " 
        ZmodN (Element of)
        ==============
        
        Implemented (parameterized) as an *Integer*. 
        This follows from the implementation of the monoid action
        "

        (let [r ;; build object and bind it
              (reify 
                co/Morphism
                  ;; morphism implementations
                  (co/morphism-impl [_]
                    ;; previledged argument un-used. 
                    ;; just return value
                    value)

                  (co/inverse [_]
                    ;; construct the inverse element
                    ;; bypass the previledged argument and just
                    ;; use value
                    (morphism (monoid-action (- value))))

                  (co/compose [x y]
                    ;; recursive definition of morphism
                    ;; uses morphism-impl
                    (morphism
                      (monoid-action
                        (co/morphism-impl x) 
                        (co/morphism-impl y))))
                  Object
                    ;; object implementations

                    (hashCode [_]
                      ;; hashCode 
                      (.hashCode value))

                    (equals [this other]
                      ;; equality testing
                      (= (.hashCode this) (.hashCode other))) 
                  )]
 
          ; TODO: this will not work for vectorized 
          ; (defmethod print-method (type r) [_ ^java.io.Writer w] 
          ;   ;; further define a print method
          ;   (.write w (str value)))

          (defmethod print-method (type r) [this ^java.io.Writer w] 
            ;; further define a print method
            (.write w (str (co/morphism-impl this))) )

          ;; finally returns the object r
          r)))))
