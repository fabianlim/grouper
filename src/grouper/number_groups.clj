(ns grouper.number-groups
  (:require [grouper.core-objects :as co]))

;; inline implementation of *Integers* over N
;; this is isomorphic *Quotient* Z / (N Z)
;; TODO : make a generic Quotient Interface ? 
(defrecord ZmodN [N]
  co/Monoid 
  (co/action [this] 
    (defn act [x y]
      " assumes both x and y are integers "
      (clojure.core/mod 
        (clojure.core/* x y)
        (:N this))))

  co/Category 
  (co/morphism-factory [this]
    (let [morphism-action (co/action this)]
      " * morphism-action binds the action "

      (defn factory [value]
        " 
        Build the ZmodN-Element. Value is the *Integer* 
        parameter from which we construct the morphism "

        (let [r ;; build object and bind it
              (reify 
                co/Morphism
                (co/action-adaptor [_]
                  ;; previledged argument un-used. 
                  ;; just return value
                  value)

                (co/compose [x y]
                  ;; recursive definition of factory
                  ;; uses action-adaptor
                  (factory
                    (morphism-action
                      (co/action-adaptor x) 
                      (co/action-adaptor y)))))]

          (defmethod print-method (type r) [x ^java.io.Writer w] 
            ;; further define a print method
            (.write w (str value)))

          ;; factory finally returns the object
          r)))))
