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

(defn print-through
  ([x]
   (do (println x)
       x))
  ([prefix x]
   (do (println prefix x)
       x)))

(defn valmap [f m]
  (zipmap (keys m)
          (map f (vals m))))

(defn key-intersection
  [dict1 dict2]
  (keep #(some->> % (find dict1) key) (keys dict2)))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

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

;;; Generate a random graph for testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn node-names
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

(defn add-edges
  [graph edges]
  (reduce (fn [acc edge]
            (uber/add-edges acc edge))
            graph
            edges))

(defn add-all-edges
  "Takes an `uber/graph` and returns an `uber/graph` with edges between all nodes."
  [graph]
  (add-edges graph (get-edges (uber/nodes graph))))

(defn locate-nodes
  [graph loc-map]
  (reduce (fn [acc node]
            (uber/add-attrs acc
                            node
                            (get loc-map node)))
          graph
          (uber/nodes graph)))

(defn randomly-locate-nodes
  "Takes an `uber/graph` and an `int` maximum value for axis values and assigns
  x, y, and z coordinates for each.
  Z coordinate is initialized to 0 as the points should all be in euclidean plane."
  [max-coord graph]
  (->> (reduce (fn [acc node]
                 (assoc acc
                        node
                        {:x (rand max-coord)
                         :y (rand max-coord)}
                        :z 0))
               {}
               (uber/nodes graph))
    (locate-nodes graph)))



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
  (->> (node-names num-nodes)
    (apply uber/graph)
    (add-all-edges)
    (randomly-locate-nodes max-coord)
    (length-graph)))

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
  ([g]
   (total-edge-weight g :length))
  ([g edge-property]
   (reduce +
           (map #(edge-value g % edge-property)
                (unidirectional-edges g)))))

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


(defn point-in-square
  [point top-left-corner bottom-right-corner]
  (and (<= (:x point) (:x bottom-right-corner))
       (>= (:x point) (:x top-left-corner))
       (<= (:y point) (:y top-left-corner))
       (>= (:y point) (:y bottom-right-corner))))

(defn points-in-square
  "Gets the set of all points which are in this square."
  [graph top-left-corner bottom-right-corner]
  (filter #(point-in-square (node-location graph %)
                            top-left-corner
                            bottom-right-corner)
          (uber/nodes graph)))

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

;; grid stuff. Something in here is wrong.

(defn grid-sizes
  "Takes an `int` number of vertices and a `float` edge length to determine the size of a grid edge.
  Lazy sequence produces multiple valid grid sizes which are chosen between.
  This value for grid sizes is under Definition 4.1 on page 4 of the garg paper."
  ([num-vertices edge-length]
   (take (Math/ceil (log2 num-vertices)) (grid-sizes num-vertices edge-length 0)))
  ([num-vertices edge-length i]
   (lazy-seq (cons (* (Math/pow 2 i) (/ edge-length num-vertices))
                   (grid-sizes num-vertices edge-length (inc i))))))

(defn get-subcells
  "Gets the four smaller cells that make up this cell."
  [top-left bottom-right]
  (let [size   (/ (- (:x bottom-right) (:x top-left)) 2)
        points [top-left
                (merge-with + top-left {:x size})
                (merge-with + top-left {:y (- size)})
                (merge-with + top-left {:x size :y (- size)})]]
    (for [p points]
      [p (merge-with + {:x size :y (- size)} p)])))

(defn cell-locations
  "Takes a `dict` center, an `int` edge-length, and a `float` grid size and produces top left
  corners of grid squares.
  This is required for determining items in grids for `G_i-potential` calculation. Definition 4.1
  on page 4 of the garg paper."
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

(defn grid-cell-map
  "Get the root and tree for grid subdivisions."
  ([num-vertices center diameter]
   (let [[tl br] (first (cell-locations center diameter diameter))]
     [[tl br] (grid-cell-map center
                             diameter
                             (/ diameter num-vertices)
                             tl
                             br
                             {})]))
  ([center diameter lower-bound tl br cell-map]
   (if (< diameter lower-bound)
     cell-map
     (let [subcells (get-subcells tl br)]
       (assoc (reduce (fn [acc [new-tl new-br]]
                        (grid-cell-map (midpoint new-tl new-br)
                                       (/ diameter 2)
                                       lower-bound
                                       new-tl
                                       new-br
                                       acc))
                      cell-map
                      subcells)
              [tl br]
              subcells)))))

;; TODO this is never used. Do we need this function?
(defn g-potential
  "takes an `uber/graph`, a `dict` square center, a `float` edge length, and a `float` grid
  size and produces a g-potential measure.
  Implementation of `G_i-potential`: Definition 4.1 on page 4 of the garg paper.
  The stand-in for `i` is the `grid-size`, which is determined by "
  [graph center edge-length grid-size]
  (* grid-size
     ;; Get the number of points in grid that contain a point of `graph`
     (count (filter (fn [[tl br]] (point-exists-in-square
                                    graph
                                    tl
                                    br))
                    (cell-locations center edge-length grid-size)))))

;; TODO this is never used. Do we need this function?
(defn set-potential
  "Denoted `P(S)` in the paper on page 4.
  Takes an `uber/graph` and an `int` k and returns sum of potentials for each grid size.
  `edge-length` = d(i,j) for the circle formed between i,j with center `center`.
  k = number of nodes"
  [graph center edge-length k]
  ;; `NOTE` This is `log(k)` instead of `log(k) -1` outlined in the paper. This is
  ;; because the paper gets the first log(k) items via indexes 0 to log(k)-1. In
  ;; this implementation we take the first log(k) explicitly.
  (->> (grid-sizes k edge-length)
      (map #(g-potential graph center k %))
      (apply +)))

(defn get-circle
  ([graph src dst]
   (get-circle (node-location graph src) (node-location graph dst)))
  ([src dst]
   [(midpoint src dst) (* (Math/sqrt 3) (lp-distance src dst))]))


(defn get-base-potential
  [{:keys [graph k]} [tl br]]
  (let [points       (points-in-square graph tl br)
        m            (count points)
        remaining    (- (inc k) m)
        k-potentials (into [] (concat [0]
                                      (take m
                                            (repeat (- (:x br) (:x tl))))
                                      (take remaining
                                            (repeat ##Inf))))]
    {:potentials k-potentials
     :points     points}))


(defn get-base-potentials
  [{:keys [graph k] :as state} cells potentials]
  (reduce (fn [acc cell]
              (assoc acc
                     cell
                     (get-base-potential state cell)))
              potentials
              cells))

(defn potential-from-children
  [{:keys [graph diameter k]} parent children potentials]
  {:potentials (into [0]
                         (for [i (range (inc k))]
                          (+ diameter
                             (apply min
                                    (map
                                      #(get-in potentials [% :potentials i])
                                      children)))))
       :points     (apply concat (map :points (map #(get potentials %) children)))})


(defn potentials-from-children
  [{:keys [graph diameter k] :as state} nodes tree potentials]
  (reduce (fn [acc node]
            (assoc acc
                   node
                   (potential-from-children
                     state
                     node
                     (get tree node)
                     acc)))
          potentials
          nodes))

(defn minpot
  ([{:keys [graph center diameter k] :as state}]
   (let [[root tree] (grid-cell-map (count (uber/nodes graph))
                                    center
                                    diameter)]
     (minpot state [root] tree {})))
  ([{:keys [graph center diameter k] :as state} current-nodes tree potentials]
   (let [children (reduce (fn [acc node] (into acc (get tree node)))
                          []
                          current-nodes)]
     (if (= 0 (count children))
       (get-base-potentials state current-nodes potentials)
       (->> (minpot state children tree potentials)
           (potentials-from-children state current-nodes tree))))))


;; End Zack's minpot -----------------------------------------

(defn test-minpot
  [num-nodes max-coords comm-range]
  (let [graph                          (weight-tree (rand-full-graph num-nodes max-coords) comm-range)
        [src dst]                      (first (get-edges (uber/nodes graph)))
        [mid diameter]                 (get-circle graph src dst)
        state                          {:graph graph :center mid :diameter diameter :k num-nodes}
        [grid-size-0 & grid-size-rest] (grid-sizes num-nodes diameter)
        num-grids                      (inc (count grid-size-rest))]
    (uber/pprint graph)
    (print "\nCircle\n")
    (print src "---" diameter "----[" mid "]--->" dst)
    (print "\nnum-grids = " num-grids)
    (print "\nfirst grid size = " grid-size-0)
    (minpot state)
    ;;(minpot-init state grid-size-0)
    ))

(defn k-min-spanning-tree
  [graph k]
  (let [ret {}]
    (for [[src dst] (get-edges (uber/nodes graph))]
      (let [[mid diameter] (get-circle src dst)
            ret            {}
            state          {:graph graph :center mid :diameter diameter :k k}]
        (if (>= (points-in-circle graph mid diameter) k)
          (->> (minpot state)
            ;; TODO create the minimum spanning tree from this min potential set.
            (minimum-spanning-tree ,,,)
            (assoc ret [src dst])))))
    ;; TODO get the minimum spanning tree with minimum weight
    (min-key ret ,,,,)))

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


(defn read-in-graph
  "Like Max's, but actually works."
  [{:keys [nodes edges]}]
  (as-> (keys nodes) $
    (apply uber/graph $)
    (locate-nodes $ nodes)
    (add-edges $ edges)
    (length-graph $)))

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
   (let [kmst     (k-min-spanning-tree graph k)
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
