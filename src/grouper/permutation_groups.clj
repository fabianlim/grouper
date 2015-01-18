(ns grouper.permutation-groups
  (:require [grouper.core-group-functions :as co]))

;; <- the protocol approach, but I double I will go this way
;; (defprotocol InjectiveMap)
;; 
;; ; TODO ; support for record
;; ; - need a custom printer
;; ; - need to enforce constraint for hashmap
;; (defrecord Permutation [x]
;;   InjectiveMap
;;   )


; compose method for permutation morphisms
(defn compose [this other]
  " compose two hash-maps describing permutations "
  ;; result is a union of two maps
  ;; - first map is (this) with values replaced by values of (other)
  ;; - second map is (other)'s keys and values that are out of (this)'s range 
  (let [this-k-with-v-mapped-by-other (map (fn [[k v]] [k (other v v)])  (seq this)) 
        this-v (vals this)
        other-k-out-of-range-of-this (filter (fn [[k v]] (not-any? #(= k %) this-v))  (seq other))
        fn-filter-moved-points #(filter (fn [[k v]] (not= k v)) %)]
    (reduce into {} [(fn-filter-moved-points this-k-with-v-mapped-by-other)
                     (fn-filter-moved-points other-k-out-of-range-of-this)])))

; cyc method for permutation morphisms
(defn cyc [& args]
  " construct permutation by specifying cycle "
  (cyc-from-list args))

; cyc method for permutation morphisms
(defn cyc-from-list [[x & xs :as all]]
  ;; the first zipmap has one element less in xs
  ;; the last element maps to first
  ;; O(n) complexity because of the last
  (if (nil? xs) 
    {} ; if xs is nil, will return empty map
    (assoc (zipmap all xs) (last xs) x)))


;; TODO write a macro to construct permutations?
;; (reduce compose[(cyc :1 :2) (cyc :2 :3) (cyc :a :b)])

;; TODO ; not very useful to have such a complicated method simply to print the
;; permutation
; print-permutation method for permutation morphisms
;; (defn print-permutation [p]
;;   " "
;;   (let [ks (keys p)
;;         helper (fn [k kr res] 
;;                  (if (contains? k kr) ;; if contains, might as well split here into k and rest
;;                    (recur (p k) (rem k kr) (concat res k))
;;                    (do 
;;                      (println res)
;;                      (start kr "")))
;;                  )]))

;; (cyc-from-list '(:a :b) )
;; (cyc :a :b)
;; (cyc [1 2] :a :b)
;; (cyc [1 2] [:a :b])

; first-cyc method for permutation morphisms
; TODO ; can generalize this to input a function to select the key
(defn first-cyc [p]
  " extract out the 'first' cycle. What 'first' means is the first key returned after application of first "
  (letfn [(helper [pt p-rem acc]
                 (if 
                   (contains? p-rem pt) 
                   (recur (p pt) (dissoc p-rem pt) (cons pt acc)) ; if pt is in the desconstructed perm, recur
                   (cyc-from-list acc)))] ; otherwise, return result (cyc)
      (helper (first (keys p)) p ()))) ; the first key = starting pt
