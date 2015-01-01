(ns grouper.core-math-structures)

(defprotocol Category
  " Category
    ========

    Abstract collection of *Morphisms*.

    * morphism-factory [this]: Given a category (this), return a factory 
      that constructs morphisms "

  (morphism-factory [this]))

(defprotocol Monoid
  " Monoid
    ======

    Abstract collection of mathematical structures and an action.
    - the action is associative
    - the action induces isomorphisms over the *Monoid*
    - contains a unique math structure that acts as the identity
  
    * action [this]: Given a Monoid (this), return a function describing 
      ismorphisms between any two Monoid structures. The function operates
      on a concrete data structure that underlies the implementation of
      the *Monoid*
  
    * identity-element-impl [this]: Given a *Monoid* (this), return 
      a concrete instance of the data structure that implements the 
      identity element 

    Notes
    =====
    - implementation of the action implicitly determines 
      the concrete implementation of the math structures
    - e.g. additive group of Z/NZ
      * residue classes of Z mod N
      * action implementation: modulo N
      * structure implementation: integers 0, 1, .. N-1
  "

  (action [this])
  (identity-element-impl [this]))

(defprotocol Morphism 
  " Morphism 
    ========

    Structure preserving map from one mathematical structure *Domain*
    to another *Codomain*

    * compose [this other]: Compose a *Morphism* (this) with another
      *Morphism* (other)

    * morphism-impl [this]: Exposes the underlying data structure that 
      implements the morphism

    * inverse [this] : Returns the inverse of a *Morphism*
    
    Notes
    =====
    - we limit to *Morphisms* that have the same *Domain* and *Codomain*
    - only this way we can define the *composition* between 
      two *Morphisms* in the same protocol
  "
  (compose [this other])
  (morphism-impl [this])
  (inverse [this])) 

;; (defprotocol Group
;;   " Group
;;     =====
;;   
;;     Inverse action 
;;   "
;;   )

;; generic function that returns an element from a math struct
;; TODO: In the future should make this a multimethod to support
;; other kinds of structures other than *Category*?
(defn element [obj value]
  " element 
    =======

    returns an element of a Collection (obj) that is parameterized by 
    (value)

  "
  (let [factory (morphism-factory obj)]
    (factory value)))

(defn identity-element [obj]
  " identity-element
    ================

    convinience function that wraps *element* to return the 
    identity element of an math structure

    * requires an implementation of the identity element
  "
  (element obj (identity-element-impl obj)))

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
