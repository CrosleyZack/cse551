(ns relay-node.core-test
  (:require [clojure.test :refer :all]
            [relay-node.core :refer :all]))
             
(def loc1 {:nodes {:a {:x 0 :y 0 :z 0}
                   :b {:x 36 :y 0 :z 0}
                   :c {:x 0 :y 12 :z 0}
                   :d {:x 24 :y 12 :z 0}
                   :e {:x 34.392 :y 6 :z 0}}
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
                  :b {:x 42 :y 0 :z 0}
                  :c {:x 0 :y 21 :z 0}
                  :d {:x 42 :y 21 :z 0}}
           :edges [[:a :b]
                   [:a :c]
                   [:a :d]
                   [:b :c]
                   [:b :d]
                   [:c :d]]})
(def loc3 {:node {:a {:x 0 :y 0 :z 0}
                  :b {:x 14 :y 0 :z 0}
                  :c {:x 28 :y 0 :z 0}
                  :d {:x 0 :y 14 :z 0}
                  :e {:x 14 :y 14 :z 0}
                  :f {:x 28 :y 14 :z 0}}
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
  
  (is (= mst1 54.2112))
  (is (= mst2 84))
  (is (= mst3 70)))

(deftest algorithm4-check
  (let [alg41 (algorithm4 graph1 2 18)
        alg42 (algorithm4 graph2 3 21)
        alg43 (algorithm4 graph3 2 21)])
  (is (= alg41 15.102))
  (is (= alg42 14))
  (is (= alg43 14))
  )


(def disjoint (ds-from [:a :b :c]))

(deftest get-canonical-check
  (is (= ds-get-canonical disjoint :a :a))
  (is (= ds-get-canonical disjoint :b :b))
  (is (= ds-get-canonical disjoint :c :c)))

(ds-union disjoint :a :b)
(deftest union
  (is (= (ds-to-map disjoint) 
         {:a [:a :b]
          :c [:c]})))

(deftest shared-root
  (is (= (ds-shared-root? disjoint :a :c) false))
  (is (= (ds-shared-root? disjoint :a :b) true)))


  
