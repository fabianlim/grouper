(ns grouper.core-group-functions)

; TODO : how to detect elements with infinite order?
(defn build-order-function
  " return the order of elem "
  [action]
  (fn [elem]
    (loop [acc (action elem elem) ord 2]
      (if (= acc elem)
        (dec ord) ; need to reduce by 1
        (recur (action acc elem) (inc ord))))))

(defn build-product-action
  " build a product action given a list of them "
  [actions]
  (fn [this other] 
    (map #(%1 %2 %3) actions this other)))

;; bfs
;; TODO: this prolly doesnt belong here
(defn bfs-builder
  [generate-new-items update-result populate-queue]
  " bfs reusable form " 
  (fn [init-result init-queue]
    (loop [result init-result
           queue (apply conj clojure.lang.PersistentQueue/EMPTY 
                       init-queue)]
      (if (empty? queue)
        result 
        (let [x (peek queue)
              new-items (generate-new-items x result)]
          (recur (update-result x result new-items)
               (reduce conj (pop queue) (populate-queue new-items))))))))
