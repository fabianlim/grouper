(ns grouper.core-objects)

(defprotocol Category
  " Category
    ========

    Abstract collection of *Morphisms* over a *Domain*.

    * morphism-factory [this]: Given a category (this), return a factory 
      that constructs morphisms
  "
  (morphism-factory [this]))

(defprotocol Monoid
  " Monoid
    ======

    Abstract collection of objects and an action.
    - the action is associative
    - the action induces isomorphisms over the *Monoid*
    - has an object (element) that acts as the identify

    * action [this]: Given a Monoid (this), return a function describing 
      ismorphisms between any two Monoid objects.  The function operates
      on a concrete data structure that underlies the implementation of
      the *Monoid*
  
    * identity-element-impl [this]: Given a *Monoid* (this), return 
      a concrete instance of the data structure that implements the 
      identify element 
  "
  (action [this])
  (identity-element-impl [this]))

(defprotocol Morphism 
  " Morphism 
    ========

    Mapping between objects within some *Domain*.

    * compose [this other]: Compose a *Morphism* (this) with another
      *Morphism* (other)

    * data [this]: Exposes the underlying data structure. Acts as an
      adaptor to the logic that implements the act
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

;; generic function that returns an element from an object that
;; collects other objects
;; TODO: In the future should make this a multimethod to support
;; other kinds of *Objects* other than *Category*
(defn element [obj value]
  " element 
    =======

    returns an elment of a Collection (obj) that is parameterized by 
    (value)

  "
  (let [factory (morphism-factory obj)]
    (factory value)))

(defn identity-element [obj]
  " identity-element
    ================

    convinience function that wraps *element* to return the 
    identity element of an Object*

    * requires an implementation of the identity element
  "
  (element obj (identity-element-impl obj)))
