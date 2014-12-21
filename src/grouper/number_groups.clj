(ns grouper.number-groups
  (:require [grouper.core-objects :as co]))

;; inline implementation of *Integers* over N
;; this is isomorphic *Quotient* Z / (N Z)
;; TODO : make a generic Quotient Interface ? 
(defrecord ZmodN [N]
  co/Monoid 
  (co/action [this] 
    (defn act [x y]
      " Action implemented over the *Integer* *Domain* using the
        mod operation.
      "
      (clojure.core/mod 
        (clojure.core/* x y)
        (:N this))))

  co/Category 
  (co/morphism-factory [this]
    (let [morphism-action (co/action this)]
      " * morphism-action binds to the action "

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
                  (co/action-adaptor [_]
                    ;; previledged argument un-used. 
                    ;; just return value
                    value)

                  (co/compose [x y]
                    ;; recursive definition of morphism
                    ;; uses action-adaptor
                    (morphism
                      (morphism-action
                        (co/action-adaptor x) 
                        (co/action-adaptor y))))
                  Object
                    ;; object implementations

                    (hashCode [_]
                      ;; hashCode 
                      (.hashCode value))

                    (equals [this other]
                      ;; equality testing
                      (= (.hashCode this) (.hashCode other))) 
                  )]

          (defmethod print-method (type r) [_ ^java.io.Writer w] 
            ;; further define a print method
            (.write w (str value)))

          ;; finally returns the object r
          r)))))
