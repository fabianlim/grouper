(ns grouper.permutation-groups
  (:require [grouper.core-objects :as co])) 


(defrecord PermutationGroup [gens]

  ;; co/Monoid
  ;; (co/action [this]
  ;;   " Action implemented as composition"
  ;;   (defn act [x y]
  ;;     
  ;;     ))
  
  co/Category
  (co/morphism-factory [this]
    (defn morphism [m]
      (let [r (morphism-boilerplate this) ])
      (reify 
        co/Morphism

        (co/compose [x y]
          ;; recursive definition of morphism
          ;; uses morphism-impl
          (morphism
            (monoid-action
              (co/morphism-impl x) 
              (co/morphism-impl y))))
      ))))
