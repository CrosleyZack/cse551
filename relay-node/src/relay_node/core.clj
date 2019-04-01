(ns relay-node.core
  (:gen-class)
  (:require [ubergraph.core :as uber]
            [clojure.math.combinatorics :as combo]
            [jordanlewis.data.union-find :as uf]))

;;; Non-domain utility functions. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn print-lines
  [& lines]
  (doseq [line lines]
    (apply println line)))

;;; Generate a random graph for testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn nodes
  "Returns node names for `int` number of nodes, default 26."
  ([]
   (nodes 26))
  ([n]
   (map (comp keyword str char)
        (range 65 (+ 65 n)))))

(defn get-edges
  "Retrieves the edges that exist in a fully connected graph with the set of nodes.
  Removes self referencial nodes."
  [nodes]
  (filter (fn [[src dst]] (not (= src dst)))
          (apply combo/cartesian-product
                 (repeat 2 nodes))))

(defn lp-distance
  "Takes two number sequences of equal lenght and produces the euclidean distance"
  ([point1 point2]
   (lp-distance point1 point2 2))
  ([point1 point2 pow]
   (reduce (fn [acc [x, y]]
             (+ acc (Math/pow (- x y) 2)))
           0
           (map vector point1 point2))))

(defn add-all-edges
  "Takes an `uber/graph` and returns an `uber/graph` with edges between all nodes."
  [graph]
  (reduce (fn [acc edge]
            (uber/add-edges acc edge))
          graph
          (get-edges (uber/nodes graph))))

(defn randomly-locate-nodes
  "Takes an `uber/graph` and an `int` maximum value for axis values and assigns
  x, y, and z coordinates for each."
  [max-coord graph]
  (reduce (fn [acc node]
            (uber/add-attrs acc
                            node
                            {:x (rand-int max-coord)
                             :y (rand-int max-coord)
                             :z (rand-int max-coord)}))
          graph
          (uber/nodes graph)))

(defn length-graph
  "Takes an `uber/graph` with (X,Y,Z) located nodes an returns an `uber/graph` with
  length attributes on edges equal to the distance between the two points."
  [graph]
  (reduce (fn [acc {:keys [src dest]
                    :as   edge}]
            (uber/add-attr acc
                           src
                           dest
                           :length
                           (lp-distance (vals (uber/attrs graph src)) (vals (uber/attrs graph dest)))))
          graph
          (uber/edges graph)))

(defn rand-full-graph
  "Create fully connected, random graph with random locations for each node and euclidean graph weights"
  [num-nodes max-coord]
  (->> (nodes num-nodes)
    (apply uber/graph)
    (add-all-edges)
    (randomly-locate-nodes max-coord)
    (length-graph)))

;;; Utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ds-from
  "Initialize an atom-backed disjoint set from the given collection."
  [coll]
  (atom (apply uf/union-find coll)))

(defn ds-get-canonical
  "Perform the get-canonical operation on a disjoint set but update
  the atom-backed representation as well to take advantage of path
  compression."
  [disj-set-atom val]
  (let [[new-repr res] (uf/get-canonical @disj-set-atom val)]
    (reset! disj-set-atom new-repr)
    res))

(defn ds-union
  "Perform the union operation on a disjoint set but update the
  atom-backed representation as well. Could simply be a reset! call
  but this mirrors atom-uf-get-canonical above which /does/ require
  more involved handling."
  [disj-set-atom u v]
  (swap! disj-set-atom uf/union u v))

(defn ds-shared-root?
  "Check if the provided elements share a root in the given disjoint
  set, updating internal representation transparently."
  [disj-set-atom u v]
  (= (ds-get-canonical disj-set-atom u)
     (ds-get-canonical disj-set-atom v)))

(defmacro ds-with
  "Rebind the prior three functions to themselves partially applied
  to the given disjoint set. Crucially, do this lexically, not for
  all subsequent calls as that would break ds-shared-root?.

  e.g. (with-disj-set foo (ds-union :a :b)) is effectively:
       (let [ds-get-canonical (partial ds-get-canonical foo)
             ds-shared-root?  (partial ds-shared-root? foo)
             ds-union         (partial ds-union foo)]
         (ds-union :a :b))"
  [disj-set-atom & body]
  `(let [~@(mapcat #(conj [%]
                          `(partial ~% ~disj-set-atom))
                   '[ds-get-canonical
                     ds-shared-root?
                     ds-union])]
     ~@body))

;;; Graph functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Alg4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn unidirectional-edges
  "Gets the unique edges in the graph (removes bidirectional edges)"
  [g]
  (->> g
    uber/edges
    (filter :mirror?)))

(defn edge-value
  "Takes a `dictionary` edge and produces a `vector` output of source and dest node names."
  ([graph edge]
   (edge-value graph edge :weight))
  ([graph edge key]
   (get (uber/attrs graph [(:src edge) (:dest edge)]) key)))

(defn sorted-edges
  "Given a graph, returns it edges sorted in increasing order."
  [graph]
  (->> graph
    (unidirectional-edges)
    (sort #(compare (edge-value graph %1) (edge-value graph %2)))))

(defn edge-canonical-form
  [g edge]
  ((juxt :src :dest #(uber/attrs g %)) edge))

(defn weight-tree
  [tree scaling-factor]
  (reduce (fn [acc {:keys [src dest]
                    :as   edge}]
            (uber/add-attr acc
                           src
                           dest
                           :weight
                           (dec (Math/ceil
                                  (/ (edge-value acc edge :length)
                                     scaling-factor)))))
          tree
          (uber/edges tree)))

(defn max-edge-by
  [g k]
  (->> g
    unidirectional-edges
    (apply max-key #(edge-value g %))))

(defn total-edge-weight
  [g]
  (reduce +
          (map #(edge-value g %)
               (unidirectional-edges g))))

(defn remove-edge
  [g e]
  (uber/remove-edges* g [e]))

(defn minimum-spanning-tree
  "Takes a `uber/graph` and returns an `uber/graph` that is its minimum spanning tree.
  Implementation of Kruskal's algorithm."
  [graph]
  (let [disj-set    (ds-from (uber/nodes graph))
        edges       (sorted-edges graph)
        empty-graph (uber/remove-edges* graph (uber/edges graph))]
    (ds-with disj-set
             (reduce (fn [acc {:keys [src dest]
                               :as   edge}]
                       (if (not (ds-shared-root? src dest))
                         (do
                           (ds-union src dest)
                           (uber/add-edges* acc [(edge-canonical-form graph edge)]))
                         acc))
                     empty-graph
                     edges))))

;;;;; Alg5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn node-location
  [graph node]
  (uber/attrs graph node))

(defn key-intersection
  [dict1 dict2]
  (keep #(some->> % (find dict1) key) (keys dict2)))

(defn midpoint
  [graph src dest]
  (let [src-loc (node-location graph src)
        dst-loc (node-location graph dest)]
    (reduce (fn [acc key]
              (assoc acc key (/ (+
                                  (get src-loc key)
                                  (get dst-loc key))
                                2)))
            {}
            (key-intersection src-loc dst-loc))))

(defn points-in-circle
  "Takes an `uber/graph` and a `dict` center and `float` diameter for a circle
  and returns the number of points in the `uber/graph` in that circle."
  [graph center diameter]
  (reduce (fn [count point]
            (if (<=
                  (lp-distance (vals center) (vals (node-location graph point)))
                  diameter)
              (inc count)
              count))
          0
          (uber/nodes graph)))

(defn point-in-square
  "Takes a `dict` point and two `dict` points indicating opposite square corners.
  Returns true if the first point is in the square contained in these two points.
  Checks if the point is contained by square by checking if the degree formed by
  the three points is greater than 90."
  [point top-left-corner bottom-right-corner]
  (let [a (lp-distance p top-left-corner)
        b (lp-distance p bottom-right-corner)
        c (lp-distance top-left-corner bottom-right-corner)
        angle (Math/acos (/
                           (- (Math/pow c 2) (Math/pow a 2) (Math/pow b 2))
                           (* -2 a b)))]
    (if (>= angle 90)
      true
      false)))

;; c^2 = a^2 + b^2 - 2ab * cos(C)
;; cos(C) = ( c^2 - a^2 - b^2 ) / (-2ab)
;; C = cos^-1 (( c^2 - a^2 - b^2 ) / (-2ab))
(defn points-in-square
  "Checks how many points fall in this square. An item
  falls in this square if the angle formed by top-left-corner,
  point, and bottom-right-corner is greater than or equal to
  90."
  [graph top-left-corner bottom-right-corner]
  (reduce (fn [acc point]
            (let [p (node-location point)]
              (if (point-in-square p top-left-corner bottom-right-corner)
                (inc acc)
                acc)))
          0
          (uber/nodes graph)))

(defn point-exists-in-square
  "returns true the first encounter of a node in the square. Else returns false."
  ([graph top-left-corner bottom-right-corner]
   (point-exists-in-square graph top-left-corner bottom-right-corner (uber/nodes graph)))
  ([graph top-left-corner bottom-right-corner nodes]
   (if (empty? nodes)
     false
     (if (point-in-square (node-location (first nodes))
                          top-left-corner
                          bottom-right-corner)
       true
       (recur graph top-left-corner bottom-right-corner (rest nodes))))))

(defn grid-sizes
  ([num-vertices edge-length]
   (grid-sizes num-vertices edge-length 1))
  ([num-vertices edge-length i]
   (lazy-seq (cons (* i (/ edge-length num-vertices))
                   (grid-sizes num-vertices edge-length (inc i))))))

(defn cell-locations
  ([center edge-length grid-size]
   (cell-locations center edge-length grid-size 0))
  ([center edge-length grid-size i]
   ;; `TODO` how do we get the location of cells in the grid.
   (lazy-seq (cons ()
                   (cell-locations center edge-length grid-size (inc i))))))

(defn g-potential
  [graph center edge-length grid-size]
  (reduce (fn [acc [top-left-corner bottom-right-corner]]
            (if (point-exists-in-square graph top-left-corner bottom-right-corner)
              (inc acc)
              acc))
            0
            (cell-locations center edge-length grid-size)))

(defn mid-potential-set
  "Get the "
  [graph center edge-length]
  (let [k (uber/count-nodes graph)]
    ;; `TODO` take the output of this reduction and return the one with minimum potential
    (-> (take-while #(<= edge-length %) (grid-sizes k edge-length))
        (reduce (fn [acc grid-size]
              (assoc acc grid-size (g-potential graph center edge-length grid-size)))
            {})
      (apply min-key val))))

(defn get-mst-option
  "Takes an `uber/graph`, an `int` k, a source `uber/node` and destination `uber/node` and
  produces an `uber/graph` minimum spanning tree or `nil`."
  [graph k src dst]
  (let [mid      (midpoint src dst)
        diameter (* (Math/sqrt 3) (lp-distance (vals src dst)))]
    (if (>= (points-in-circle graph mid diamater) k)
      ;; Suppose a square with side of length `diameter` centered around `mid`
      (min-potential-set graph mid diameter)
      ;; `TODO` write this section - get minimum potential set and the accompanying minimum spanning tree as an `uber/graph`.
      ()
      nil)))

(defn k-minimum-spanning-tree
  "Takes a `uber/graph` and an `int` value `k` and returns an `uber/graph` that is the
  minimum spanning tree with k vertices"
  [graph k]
  (->> graph
        (uber/nodes ,,,)
        (get-edges ,,,)
        (reduce (fn [acc [src dst]]
                  (let [option (get-mst-option graph k src dst)]
                    (if option
                      (conj acc option)
                      acc)))
                []
                ,,,)
        (min-key total-edge-weight ,,,)))

;;; IO Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-graph
  "Takes a `str` file location and returns the `uber/graph` specified by the file."
  [location]
  nil)

;;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn algorithm4
  "Algorithm 4 from the paper. Takes an `uber/graph` and returns an `uber/graph`
  representing a placement of relay nodes with the minimum number of connected
  components."
  [graph comm-range budget]
  (let [mst      (minimum-spanning-tree graph)
        weighted (atom (weight-tree mst comm-range))]
    (while (> (total-edge-weight @weighted)
              budget)
      (swap! weighted
             remove-edge
             (max-edge-by @weighted :weight)))
    @weighted))

(defn algorithm5
  "Algorithm 5 from the paper. Takes an `uber/graph` and returns an `uber/graph`
  representing the placement of relay nodes with the maximum connected component
  size."
  ([graph comm-range budget]
   (algorithm5 graph comm-range budget (uber/count-nodes graph)))
  ([graph comm-range budget k]
   (let [kmst     (k-minimum-spanning-tree graph k)
         weighted (weight-tree kmst comm-range)]
     (if (> (total-edge-weight weighted) budget)
       weighted
       (recur graph comm-range budget (dec k))))))
;; (-> graph
;;   (k-minimum-spanning-tree k)
;;   (weight-tree comm-range)
;;   (if (> (total-edge-weight ))))))

(defn -main
  " Read in graphs and run algorithms. "
  [& args]
  (println "Hello, World!"))
