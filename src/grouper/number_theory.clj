(ns grouper.number-theory)

(defn extended-Euclidean
  [a b]
  "
  function returns [gcd, u, v] where u * a + v *b = gcd

  Consider the matrix product
  |_ _ b  |   = |0  1 | | 1 0 a |    
  |_ _ r1 |     |1 -q1| | 0 1 b |  
  - note r1 is the first remainder. 
  - in general the two LHS values will be r_{i-1} and r_i
  - when the bottom (later) r equals zero, the above one is the gcd
  - then it is clear the matrix on the LHS (indicated by _) 
  times [a b ] equals [gdb _]
  - so the first row of the (_) matrix gives the coefs [u v]
  "
  (letfn
    [(vec-times-const [v k] (map #(* %  k) v))
     (helper
      [Q r1 r2]
      " Q is the matrix"
      (let 
        [[R1 R2] Q ;; destructure rows
         [u v] R1
         ]
        (if 
          (= r2 0) ; check if this remainder is zero
          [r1 u v] ;; return result
          (recur [R2 (vec (map - R1 
                               (vec-times-const R2 (quot r1 r2))))] 
                 r2 (rem r1 r2))
          )))]

    ;; main running
    (let [[g u v :as result] (helper [[1 0] [0 1]] a b)]  
      (if (pos? g) ; reflect the result if g < 0
        result
        (map - result))))) 

;; test extended-Euclidean
;; (let [a 32
;;       b -82
;;       [g u v :as res] (extended-Euclidean a b)
;;       ]
;;   (println res)
;;   (println (= (+ (* u a) (* v b)) g)))
