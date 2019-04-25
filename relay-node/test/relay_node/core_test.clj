(ns relay-node.core-test
  (:require [clojure.test :refer :all]
            [relay-node.core :refer :all]
            [ubergraph.core :as uber]))

(def loc0 [{:id :a :x 0 :y 0 :z 0}
           {:id :b :x 10 :y 0 :z 0}
           {:id :c :x 0 :y 10 :z 0}])

(def loc1 [{:id :a :x 0 :y 0 :z 0}
           {:id :b :x 36 :y 0 :z 0}
           {:id :c :x 0 :y 12 :z 0}
           {:id :d :x 24 :y 12 :z 0}
           {:id :e :x 34.392 :y 6 :z 0}])

(def loc2 [{:id :a :x 0 :y 0 :z 0}
           {:id :b :x 42 :y 0 :z 0}
           {:id :c :x 0 :y 21 :z 0}
           {:id :d :x 42 :y 21 :z 0}])

(def loc3 [{:id :a :x 0 :y 0 :z 0}
           {:id :b :x 14 :y 0 :z 0}
           {:id :c :x 28 :y 0 :z 0}
           {:id :d :x 0 :y 14 :z 0}
           {:id :e :x 14 :y 14 :z 0}
           {:id :f :x 28 :y 14 :z 0}])

(defn mst
  [loc]
  (-> loc
    (init-graph ##Inf)
    (minimum-spanning-tree :length)))

(defn mst-weight
  [loc]
  (-> loc
    mst
    (total-edge-weight :length)))

(deftest mst-cal
  (is (= 20.0 (mst-weight loc0)))
  (is (=  54 (Math/round (mst-weight loc1))))
  (is (= 84.0 (mst-weight loc2)))
  (is (=  70.0 (mst-weight loc3))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn testing-graph
  [graph-params comm-range]
  (as-> graph-params $
    (apply uber/graph $)
    (length-graph $)
    (weight-forest $ comm-range)))

(deftest algorithm4-test
  ;; loc0: comm-range is 5 and budget is 20
  (let [comm-range 10]
    (is (= (algorithm4 (init-graph loc0 comm-range) comm-range 20)
           (testing-graph '([:a {:x 0 :y 0 :z 0}] [:b {:x 10 :y 0 :z 0}] [:c {:x 0 :y 10 :z 0}] [:a :b] [:a :c]) comm-range))))
  (let [comm-range 12]
    (is (= (algorithm4 (init-graph loc1 comm-range) comm-range 18)
           (testing-graph '([:a {:x 0 :y 0 :z 0}] [:b {:x 36 :y 0 :z 0}] [:c {:x 0 :y 12 :z 0}] [ :d {:x 24 :y 12 :z 0}] [:e {:x 34.392 :y 6 :z 0}] [:a :c] [:d :e] [:b :e]) comm-range))))
  (let [comm-range 21]
    (is (= (algorithm4 (init-graph loc2 comm-range) comm-range 21)
           (testing-graph '([:a {:x 0 :y 0 :z 0}] [:b {:x 42 :y 0 :z 0}] [:c {:x 0 :y 21 :z 0}] [:d {:x 42 :y 21 :z 0}] [:a :c] [:b :d]) comm-range))))
  (let [comm-range 14]
    (is (= (algorithm4 (init-graph loc3 comm-range) comm-range 21)
           (testing-graph '([:a {:x 0 :y 0 :z 0}] [:b {:x 14 :y 0 :z 0}] [:c {:x 28 :y 0 :z 0}] [:d {:x 0 :y 14 :z 0}] [:e {:x 14 :y 14 :z 0}] [:f {:x 28 :y 14 :z 0}] [:a :b] [:a :d] [:b :c]) comm-range)))))

;;(deftest algorithm4-check
   ;;(let [alg41 (algorithm4 (mst loc1) 2 18)
       ;;      alg42 (algorithm4 (mst loc2) 3 21)
     ;;        alg43 (algorithm4 (mst loc3) 2 21)])
   ;;(is (= alg41 15.102))
   ;;(is (= alg42 14))
   ;;(is (= alg43 14))
;; )

(defn result
  [loc comm-range budget]
  (-> loc
    init-graph
    (weight-forest comm-range)
    (#(do (uber/viz-graph % {:auto-label true}) %))
    (algorithm4 comm-range budget)
    (uber/viz-graph {:auto-label true})))


(def disjoint (ds-from [:a :b :c]))

(deftest get-canonical-check
  (is (= (ds-get-canonical disjoint :a) :a))
  (is (= (ds-get-canonical disjoint :b) :b))
  (is (= (ds-get-canonical disjoint :c) :c)))

(ds-union disjoint :a :b)
(deftest union
  (is (= (ds-to-map disjoint)
         {:a [:a :b]
          :c [:c]})))

(deftest shared-root
  (is (= (ds-shared-root? disjoint :a :c) false))
  (is (= (ds-shared-root? disjoint :a :b) true)))
