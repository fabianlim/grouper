(ns grouper.permutation-groups
  (:require [grouper.core-group-functions :as co]
            [grouper.free-groups :as fg]))


; compose method for permutation morphisms
(defn compose [this other]
  " compose two hash-maps describing permutations "
  ;; result is a union of two maps
  ;; - first map is (this) with values replaced by values of (other)
  ;; - second map is (other)'s keys and values that are out of (this)'s range 
  (let [this-k-with-v-mapped-by-other 
          (map (fn [[k v]] [k (other v v)])  (seq this)) 
        this-v (vals this)
        other-k-out-of-range-of-this 
          (filter (fn [[k v]] (not-any? #(= k %) this-v))  (seq other))
        filter-moved-points #(filter (fn [[k v]] (not= k v)) %)]
    (reduce into {} [(filter-moved-points this-k-with-v-mapped-by-other)
                     (filter-moved-points other-k-out-of-range-of-this)])))

; order function for permutations
; TODO: how to organize such functions?
(def order (co/build-order-function compose))

; cyc method for permutation morphisms
(defn cyc [& args]
  " construct permutation by specifying cycle "
  ;; the first zipmap has one element less in xs
  ;; the last element maps to first
  ;; O(n) complexity because of the last
  (when (and (not (nil? args)) (apply distinct? args))
    (let [[x & xs] args]
      (if (nil? xs) 
        {} ; if xs is nil, will return empty map
        (assoc (zipmap args xs) (last xs) x)))))

;; inverse 
(def inverse clojure.set/map-invert)

;; cyc-notation alternative impl
(defn cyc-notation [& args]
  (loop [acc (hash-map)
         [x & xs] args]
    (if (nil? x)
      acc
      (recur (if (some #(contains? acc %) x)
             acc
             (into acc (apply cyc x)))
           xs))))

;; build-cyc-transform
(defn- build-cyc-transform [aggregate-func]
  " build a function that traces a cycle and applies some
    aggregation on it"
  (fn [perm point]
    (loop [pt point
           p-rem perm
           acc (vector)]
      (if (contains? p-rem pt) 
        (recur (perm pt) (dissoc p-rem pt) (aggregate-func pt acc)) 
        acc))))   ; otherwise, return result (cyc)
      
" extract out the cycle that point is involved in "
;; TODO: this returns a vector
(def point->path (build-cyc-transform #(conj %2 %1)))

" extract out the cycle (in reverse) that point is involved in "
;; TODO: this returns a list
(def point->reverse-path (build-cyc-transform #(cons %1 %2)))

;; cyc-from-point
(defn cyc-from-point [perm point]
  " return the cyc that point is involved in perm "
  (apply cyc (point->path perm point)))

;; delete-cyc
(defn delete-cyc [perm cyc]
  " delete cyc from perm "
   (apply dissoc perm (keys cyc)))

;; permutation->cyc-paths
(defn permutation->cyc-paths [p]
  " destruct to set of cycles "
  (if (empty? p)
    #{}
    (let [pp (point->path p (first (keys p)))
          c (apply cyc pp)
          p-rem (delete-cyc p c)]
      (conj (permutation->cyc-paths p-rem) pp))))

;; pointwise-stabilizer?
(defn pointwise-stabilizer? 
  [p S]
  " is permutation p a pointwise stabilizer of points S " 
    (= (map #(p % %) S) S))  

;; moved-points-other-than
(defn moved-points-other-than 
  [p v]
  " find moved points of p not in v "
  (filter #(not (contains? (set v) %)) (keys p)))
