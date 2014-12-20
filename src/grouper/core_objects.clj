(ns grouper.core-objects)

(defprotocol Category
  " Category
    ========

    Abstract collection of *Morphisms* over a *Domain*.

    * morphism-factory [this]: Given a category (this), return a factory 
      that constructs morphisms
  "
  (morphism-factory [this])
  )


(defprotocol Monoid
  " Monoid
    ======

    Abstract collection of objects and an action.
    The action induces isomorphisms over the abstract collection.

    * action [this]: Given a Monoid (this), return a function describing 
      ismorphisms between any two Monoid objects.
  "
  (action [this]))
  
(defprotocol Morphism 
  " Morphism 
    ========

    Mapping between objects within some *Domain*.

    * compose [this other]: Compose a *Morphism* (this) with another
      *Morphism* (other)

    * action-adaptor [this]: Used when _compose's_ implementation 
      depends on an _action_ implemented somewhere else, and _action_
      expects data in a different form.
      Implement the adaptor here.
  "
  (compose [this other])
  (action-adaptor [data]))

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
