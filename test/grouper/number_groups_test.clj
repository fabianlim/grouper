(ns grouper.number-groups-test
  (:require [clojure.test :refer :all]
            [grouper.number-groups :refer :all]
            [grouper.core-objects :as co]))

(defn elem [x] (co/element (->ZmodN 5) x)) 

;; testing ZmodN
(testing "ZmodN"
  (let [N 5
        elem (fn [x] (co/element (->ZmodN N) x))]
    (is (= (co/compose 
          (elem 3) 
          (elem 5)) 
          (elem 3)))
    ))
