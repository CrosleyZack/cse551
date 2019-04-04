(ns relay-node.core
  (:gen-class)
  (:require [ubergraph.core              :as uber]
            [clojure.math.combinatorics  :as combo]
            [clojure.math.numeric-tower  :as math]
            [jordanlewis.data.union-find :as uf]
            [clojure.string              :as str]
            [clojure.edn                 :as edn]
            [clojure.core.match          :refer [match]]))

;;; Non-domain utility functions. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn print-lines
  [& lines]
  (doseq [line lines]
     (apply println line)))

(defn valmap [f m]
  (zipmap (keys m)
          (map f (vals m))))

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

(defn add-all-edges
  "Takes an `uber/graph` and returns an `uber/graph` with edges between all nodes."
  [graph]
  (reduce (fn [acc edge]
            (uber/add-edges acc edge))
          graph
          (get-edges (uber/nodes graph))))

(defn randomly-locate-nodes
  "Takes an `uber/graph` and an `int` maximum value for axis values and assigns
  x, y, and z coordinates for each.
  Z coordinate is initialized to 0 as the points should all be in euclidean plane."
  [max-coord graph]
  (reduce (fn [acc node]
            (uber/add-attrs acc
                            node
                            {:x (rand max-coord)
                             :y (rand max-coord)
                             :z 0}))
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
                           (lp-distance (uber/attrs graph src) (uber/attrs graph dest))))
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

(defn ds-to-map
  [ds-atom]
  (edn/read-string (.toString @ds-atom)))

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

(defn coords-seq
  [m]
  ((juxt :x :y :z) m))

(defn raise-to
  [pow n]
  (math/expt n pow))

(def reciprocal (partial / 1))

(defn lp-distance
  "Takes two number sequences of equal length and produces the euclidean distance"
  ([point1 point2]
   (lp-distance point1 point2 2))
  ([point1 point2 pow]
   (->> (merge-with - point1 point2)
     vals
     (map (partial raise-to pow))
     (reduce +)
     (raise-to (reciprocal pow)))))

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

;; `TODO` figure out why this is producing bad results - all edges attached to first node.
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
  [src dst]
  (valmap #(/ % 2) (merge-with + src dst)))

(defn graph-midpoint
  [graph src dst]
  (midpoint
    (node-location graph src)
    (node-location graph dst)))

(defn points-in-circle
  "Takes an `uber/graph` and a `dict` center and `float` diameter for a circle
  and returns the number of points in the `uber/graph` in that circle."
  [graph center diameter]
  (let [c (vals center)]
   (reduce (fn [count point]
            (if (<=
                  (lp-distance c (node-location graph point))
                  diameter)
              (inc count)
              count))
          (uber/nodes graph))))

(defn clamp
  [lower upper num]
  (match ((juxt (partial < lower)
                (partial > upper))
          num)
    [true  true ] num
    [false _    ] lower
    [_     false] upper))

;; c^2 = a^2 + b^2 - 2ab * cos(C)
;; cos(C) = ( c^2 - a^2 - b^2 ) / (-2ab)
;; C = cos^-1 (( c^2 - a^2 - b^2 ) / (-2ab))
(defn point-in-square
  "Takes a `dict` point and two `dict` points indicating opposite square corners.
  Returns true if the first point is in the square contained in these two points.
  Checks if the point is contained by square by checking if the angle formed by
  the three points is at least 90 degrees.
  NOTE All three points must be in the same plane. If not, this won't produce proper results."
  [point top-left-corner bottom-right-corner]
  (let [a (lp-distance point top-left-corner)
        b (lp-distance point bottom-right-corner)
        c (lp-distance top-left-corner bottom-right-corner)]
    (->> (/
          (- (math/expt c 2) (math/expt a 2) (math/expt b 2))
          (* -2 a b))
      (clamp -1 1)
      (Math/acos)
      (< (/ Math/PI 2)))))

(defn points-in-square
    "Checks how many points fall in this square. An item
  falls in this square if the angle formed by top-left-corner,
  point, and bottom-right-corner is greater than or equal to
  90."
  [graph top-left-corner bottom-right-corner]
  (count (filter #(point-in-square (node-location graph %)
                                   top-left-corner
                                   bottom-right-corner)
                 (uber/nodes graph))))

(defn point-exists-in-square
  "returns true the first encounter of a node in the square. Else returns false."
  ([graph top-left-corner bottom-right-corner]
   (point-exists-in-square graph top-left-corner bottom-right-corner (uber/nodes graph)))
  ([graph top-left-corner bottom-right-corner nodes]
   (some #(point-in-square
            %
            top-left-corner
            bottom-right-corner)
         (map #(node-location graph %) nodes))))

(defn grid-sizes
  "Takes an `int` number of vertices and a `float` edge length to determine the size of a grid edge.
  Lazy sequence produces multiple valid grid sizes which are chosen between."
  ([num-vertices edge-length]
   (grid-sizes num-vertices edge-length 1))
  ([num-vertices edge-length i]
   (lazy-seq (cons (* i (/ edge-length num-vertices))
                   (grid-sizes num-vertices edge-length (inc i))))))

(defn cell-locations
  "Takes a `dict` center, an `int` edge-length, and a `float` grid size and produces top left
  corners of grid squares."
  ([center edge-length grid-size]
   (letfn [(gen-deltas [loc i j]
             (cond
               (= loc :NW) [(* i grid-size)
                            (- (* j grid-size))
                            0]
               ;; The southeast corner of the cell at i,j is also the northwest corner of the cell at i+1,j+1.
               (= loc :SE) (recur :NW (inc i) (inc j))))
           (delta-merge [m deltas]
             (merge-with +
                         m
                         (zipmap [:x :y :z]
                                 deltas)))]
     (let [count    (/ edge-length grid-size)
           halved   (/ edge-length 2)
           top-left (delta-merge center
                                 ((juxt - identity (constantly 0)) halved))]
       (for [i (range count)
             j (range count)]
         [(delta-merge top-left
                       (gen-deltas :NW i j))
          (delta-merge top-left
                       (gen-deltas :SE i j))])))))

(defn g-potential
  "takes an `uber/graph`, a `dict` square center, a `float` edge length, and a `float` grid
  size and produces a g-potential measure."
  [graph center edge-length grid-size]
  (do
   (print "\nG-Potential Called with :")
   (print "\ncenter = " center)
   (print "\nedge-length = " edge-length)
   (print "\ngrid-size = " grid-size)
   (reduce (fn [acc [top-left-corner bottom-right-corner]]
            (if (point-exists-in-square graph top-left-corner bottom-right-corner)
              (inc acc)
              acc))
          0
          (cell-locations center edge-length grid-size))))

(defn min-potential-set
  "Get the minimum potential item from the options. Still not sure what this really means...
  `TODO` Figure out what this really means."
  [graph center edge-length]
  (let [k (uber/count-nodes graph)]
    ;; `TODO` take the output of this reduction and return the one with minimum potential
    ;; `TODO` this is the wrong way to use take-while....
    (->> (take-while #(<= % edge-length) (grid-sizes k edge-length))
         (reduce (fn [acc grid-size]
                (assoc acc grid-size (g-potential graph center edge-length grid-size)))
              {})
         (apply min-key val))))

(defn get-mst-option
  "Takes an `uber/graph`, an `int` k, a source `uber/node` and destination `uber/node` and
  produces an `uber/graph` minimum spanning tree or `nil`."
  [graph k src dst]
  (let [mid      (midpoint src dst)
        diameter (* (Math/sqrt 3) (lp-distance src dst))]
    (if (>= (points-in-circle graph mid diameter) k)
      ;; Suppose a square with side of length `diameter` centered around `mid`
      (min-potential-set graph mid diameter)
      ;; `TODO` write this section - get minimum potential set and the accompanying minimum spanning tree as an `uber/graph`.
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

(defn tokenize
  [s]
  (str/split s #" +"))

(defn parse-node
  [node-line]
  (let [[node & coords] (tokenize node-line)
        [x y z]         (map edn/read-string coords)]
    {:id (keyword node)
     :x  x
     :y  y
     :z  z}))

(defn parse-edge
  [edge-line]
  (let [[a b] (tokenize edge-line)]
    [(keyword a)
     (keyword b)]))

(defn parse-graph
  "Parses a graph from a string with: node lines with an id as well as x, y, and z coordinates separated by spaces; an empty separator line; edge lines with two node ids. Returns a map of with keys nodes, a map of node ids to :x, :y, and :z coordinates, and edges, a seq of 2-element sequences of node ids."
  [graph-str]
  (let [[nodes edges] (map str/split-lines
                           (str/split graph-str #"\n\n"))
        nodes         (reduce (fn [acc {:keys [id x y z]
                                        :as   node}]
                                (assoc acc
                                       id
                                       node))
                              {}
                              (map parse-node nodes))
        edges         (map parse-edge edges)]
    {:nodes nodes
     :edges edges}))

(defn make-init-forms
  "Takes a map with keys nodes, a map of node ids to :x, :y, :z coords, and edges, a seq of 2-element sequences of node ids. Returns a seq of [src dst metadata] vectors which can be passed as arguments to uber/graph."
  [{:keys [nodes edges]}]
  (map (fn [[src dst]]
         [src
          dst
          {:length (apply lp-distance
                          (map #(get nodes %)
                               [src dst]))}])
       edges))

(defn instantiate-graph
  [edges]
  (apply uber/graph edges))

(defn read-graph
  "Takes a `str` file location and returns the `uber/graph` specified by the file."
  [location]
  (-> location
    slurp
    parse-graph
    make-init-forms
    instantiate-graph))

;;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn algorithm4
  "Algorithm 4 from the paper. Takes an `uber/graph` and returns an `uber/graph`
  representing a placement of relay nodes with the minimum number of connected
  components."
  [graph comm-range budget]
  (let [graph (weight-tree graph comm-range)
        mst   (atom (minimum-spanning-tree graph))]
    (while (> (total-edge-weight @mst)
              budget)
      (swap! mst
             remove-edge
             (max-edge-by @mst :weight)))
    @mst))

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
