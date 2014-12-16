(ns grouper.group)


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
    The action induces a isomorphisms over the abstract collection.

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


;; inline implementation of *Integers* over N
;; this is isomorphic *Quotient* Z / (N Z)
;; TODO : make a generic Quotient Interface ? 
(defrecord ZmodN [N]
  Monoid 
  (action [this] 
    (defn act [x y]
      " assumes both x and y are integers "
      (clojure.core/mod 
        (clojure.core/* x y)
         (:N this))))

  Category 
  (morphism-factory [this]
    (let [morphism-action (action this)]
      " * morphism-action binds the action "
      (defn factory [value]
        " value is the *Integer* parameter from which we construct 
          the morphism 
        "
        (reify 
          Morphism
          (action-adaptor [x]
            " x = this "
            value)
          (compose [x y]
            " x = this and y = other "
            (morphism-action
              (action-adaptor x) 
              (action-adaptor y))))))))

;; generic function that returns an
;; TODO: what about objects that are non-morphisms?
(defn element [obj value]
  " element 
    =======

    returns an elment of a Collection (obj) that is parameterized by 
    (value)

  "
  (let [factory (morphism-factory obj)]
    (factory value)))

(compose (element (ZmodN. 5) 3) (element (ZmodN. 5) 2))
