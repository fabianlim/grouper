(ns grouper.number-groups-test
  (:require [clojure.test :refer :all]
            [grouper.number-groups :refer :all]
            [grouper.core-objects :as co]))


(testing "Compose"
  (is (co/compose 
        (co/element (->ZmodN 5) 3) 
        (co/element (->ZmodN 5) 2)) 1))
