(ns grouper.permutation-groups
  (:require [grouper.core-group-functions :as co]))


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
  ;; the first zipmap has one element less in xs
  ;; the last element maps to first
  ;; O(n) complexity because of the last
  (let [[x & xs :as all] args]
    (when (apply distinct? all)
      (if (nil? xs) 
        {} ; if xs is nil, will return empty map
        (assoc (zipmap all xs) (last xs) x)))))

;; cyc examples
;; (apply cyc [1 2])
;; (cyc 1 2 1)

;; cycle-notation
;; if a cycle is improperly specified, will simply ignore it
;; (defn cycle-notation [& args]
;;   (reduce compose (map (partial apply cyc) args)))

;; cyc-notation alternative impl
(defn cycle-notation [& args]
  (letfn [(update-acc [acc x]
            (if (some #(contains? acc %) x)
              acc
              (into acc (apply cyc x))))
          (helper [acc [x & xs]]
            (if (nil? xs)
              (update-acc acc x)
              (recur (update-acc acc x) xs)))]
    (helper {}  args)))

; first-cyc method for permutation morphisms
; TODO ; can generalize this to input a function to select the key
(defn first-cyc [p]
  " extract out the 'first' cycle. What 'first' means is the first key returned after application of first "
  (letfn [(helper [pt p-rem acc]
                 (if 
                   (contains? p-rem pt) 
                   (recur (p pt) (dissoc p-rem pt) (cons pt acc)) ; if pt is in the desconstructed perm, recur
                   (cyc-from-collection acc)))] ; otherwise, return result (cyc)
      (helper (first (keys p)) p ()))) ; the first key = starting pt
