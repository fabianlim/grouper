(ns grouper.number-theory)

(defn extended-euclidean
  [a b]
  "
  function returns [gcd, u, v] where u * a + v *b = gcd

  Consider the matrix product
  |_ _ b  |   = |0  1 | | 1 0 a |    
  |_ _ r1 |     |1 -q1| | 0 1 b |  
  - note r1 is the first remainder. 
  - in general the two LHS values will be r_{i-1} and r_i
  - when the bottom (latter) r equals zero, the above one is the gcd
  - then it is clear the matrix on the LHS (indicated by _) 
  times [a b ] equals [gcd _]
  - so the first row of the (_) matrix gives the coefs [u v]
  "
  (loop [[[u v :as R1] R2] [[1 0] [0 1]] ;; (_) matrix 
         r1 a 
         r2 b]
         (if (= r2 0) ; check if this remainder is zero
           (if (pos? r1)
             (vector r1 u v) ;; return result. [gcd, u, v]
             (map #(* % -1) (vector r1 u v)))  ;; if r1 < 0, have to flip it
           (recur (vector R2 (map - R1 (map #(* % (quot r1 r2)) R2)))  ;; new (_) matrix
                   r2  ;; second arg
                   (rem r1 r2))))) ;; third arg

;; test extended-Euclidean
;; (let [a -4 
;;       b -8
;;       [g u v :as res] (extended-euclidean a b)]
;;   (println res)
;;   (println (= (+ (* u a) (* v b)) g)))
