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
        " value is the *Integer* parameter from which we construct 
          the morphism 
        "
        (reify 
          co/Morphism
          (co/action-adaptor [x]
            " x = this "
            value)
          (co/compose [x y]
            " x = this and y = other "
            (factory
              (morphism-action
                (co/action-adaptor x) 
                (co/action-adaptor y)))))))))

; (defmethod print-method ZmodN [x ^java.io.Writer writer]
;   (print-method (:
