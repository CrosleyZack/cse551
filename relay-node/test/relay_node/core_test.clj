(ns relay-node.core-test
  (:require [clojure.test :refer :all]
            [relay-node.core :refer :all]))
             
(def loc1 {:nodes {:a {:x 0 :y 0 :z 0}
                   :b {:x 3 :y 0 :z 0}
                   :c {:x 0 :y 1 :z 0}
                   :d {:x 2 :y 1 :z 0}
                   :e {:x 1.8660 :y 0.5 :z 0}}
           :edges [[:a :b]
                   [:a :c]
                   [:a :d]
                   [:a :e]
                   [:b :c]
                   [:b :d]
                   [:b :e]
                   [:c :d]
                   [:c :e]
                   [:d :e]]})
(def loc2 {:node {:a {:x 0 :y 0 :z 0}
                  :b {:x 2 :y 0 :z 0}
                  :c {:x 0 :y 1 :z 0}
                  :d {:x 2 :y 1 :z 0}}
           :edges [[:a :b]
                   [:a :c]
                   [:a :d]
                   [:b :c]
                   [:b :d]
                   [:c :d]]})
(def loc3 {:node {:a {:x 0 :y 0 :z 0}
                  :b {:x 1 :y 0 :z 0}
                  :c {:x 2 :y 0 :z 0}
                  :d {:x 0 :y 1 :z 0}
                  :e {:x 1 :y 1 :z 0}
                  :f {:x 2 :y 1 :z 0}}
           :edges [[:a :b]
                   [:a :c]
                   [:a :d]
                   [:a :e]
                   [:a :f]
                   [:b :c]
                   [:b :d]
                   [:b :e]
                   [:b :f]
                   [:c :d]
                   [:c :e]
                   [:c :f]
                   [:d :e]
                   [:d :f]
                   [:e :f]]})


   
(deftest mst-cal
  (let [graph1 (instantiate-graph (make-init-forms loc1))
        graph2 (instantiate-graph (make-init-forms loc2))
        graph3 (instantiate-graph (make-init-forms loc3))])
  (let [mst1 (minimum-spanning-tree graph1)
        mst2 (minimum-spanning-tree graph2)
        mst3 (minimum-spanning-tree graph3)]
  
  (is (= mst1 5))
  (is (= mst2 3))
  (is (= mst3 6)))

(deftest algorithm4-check
  (let [alg41 (algorithm4 graph1 1 3)
        alg42 (algorithm4 graph2 1 3)
        alg43 (algorithm4 graph3 1 3)])
  (is (= alg41 2))
  (is (= alg42 2))
  (is (= alg43 2))
  )
