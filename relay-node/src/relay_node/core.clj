(ns relay-node.core
  (:gen-class)
  (:require [ubergraph.core              :as uber]
            [clojure.math.combinatorics  :as combo]
            [clojure.math.numeric-tower  :as math]
            [jordanlewis.data.union-find :as uf]
            [clojure.string              :as str]
            [clojure.edn                 :as edn]
            [clojure.core.match          :refer [match]]
            [clojure.tools.cli :refer [parse-opts]]))

;;; Non-domain utility functions. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn first-defined
  [& args]
  (some identity args))

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

(defn midpoint
  [src dst]
  (valmap #(/ % 2) (merge-with + src dst)))

(defn point-in-square
  [point top-left-corner bottom-right-corner]
  (and (<= (:x top-left-corner)     (:x point))
       (< (:x point)                (:x bottom-right-corner))
       (<= (:y bottom-right-corner) (:y point))
       (< (:y point)                (:y top-left-corner))))

;;; Set Utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; Graph Utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-edges
  "Retrieves the edges that exist in a fully connected graph with the set of nodes.
  Removes self referencial nodes."
  [nodes]
  (combo/combinations nodes 2))

(defn add-edges
  [graph edges]
  (reduce (fn [acc edge]
            (uber/add-edges acc edge))
          graph
          edges))

(defn node-location
  [graph node]
  (uber/attrs graph node))

(defn locate-nodes
  [graph loc-map]
  (reduce (fn [acc node]
            (uber/add-attrs acc
                            node
                            (get loc-map node)))
          graph
          (uber/nodes graph)))

(defn induced-subgraph
  [g vertices]
  (->> g
    uber/edges
    (filter (complement #(->> %
                           ((juxt :src :dest))
                           (every? vertices))))
    (uber/remove-edges* g)))

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
  ([graph]
   (sorted-edges graph :length))
  ([graph metric]
   (->> graph
     (unidirectional-edges)
     (sort #(compare (edge-value graph %1 metric) (edge-value graph %2 metric))))))

(defn total-edge-weight
  ([g]
   (total-edge-weight g :length))
  ([g edge-property]
   (reduce +
           (filter some?
                   (map #(edge-value g % edge-property)
                        (unidirectional-edges g))))))

(defn max-edge-by
  [g k]
  (->> g
    unidirectional-edges
    (apply max-key #(edge-value g %))))

(defn remove-edge
  [g e]
  (uber/remove-edges* g [e]))

(defn weight-forest
  [forest scaling-factor]
  (reduce (fn [acc {:keys [src dest]
                    :as   edge}]
            (uber/add-attr acc
                           src
                           dest
                           :weight
                           (dec (Math/ceil
                                  (/ (edge-value acc edge :length)
                                     scaling-factor)))))
          forest
          (uber/edges forest)))

(defn validate-graph
  [graph]
  (assert (uber/ubergraph? graph))
  (assert (every?
            #(->> %
               (uber/attrs graph)
               vals
               (not-any? nil?))
            (uber/nodes graph)))
  true)


;;; Generate a random graph for testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn node-names
  "Returns node names for `int` number of nodes, default 26."
  ([]
   (node-names 26))
  ([n]
   (map (comp keyword str char)
        (range 65 (+ 65 n)))))

(defn add-all-edges
  "Takes an `uber/graph` and returns an `uber/graph` with edges between all nodes."
  [graph]
  (add-edges graph (combo/combinations (uber/nodes graph)
                                       2)))


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

(defn complete-graph [g]
  (length-graph (add-all-edges g)))


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
     :z  (first-defined z 0)}))

(defn parse-edge
  [edge-line]
  (let [[a b] (tokenize edge-line)]
    [(keyword a)
     (keyword b)]))

(defn parse-graph
  "Parses a graph from a string with: node lines with an id as well as x, y, and z coordinates separated by spaces; an empty separator line; edge lines with two node ids. Returns a map of with keys nodes, a map of node ids to :x, :y, and :z coordinates, and edges, a seq of 2-element sequences of node ids."
  [graph-str]
  (map parse-node (str/split-lines graph-str)))


(defn init-graph
  "Like Max's, but actually works."
  [nodes]
  (->> nodes
    (map (juxt :id #(dissoc % :id)))
    (apply uber/graph)))

(defn make-graph
  [graph-str]
  (-> graph-str
    parse-graph
    init-graph))

(defn read-graph
  "Takes a `str` file location and returns the `uber/graph` specified by the file."
  [location]
  (-> location
    slurp
    make-graph))

;;;;; Alg4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn edge-canonical-form
  [g edge]
  ((juxt :src :dest #(uber/attrs g %)) edge))

(defn minimum-spanning-tree
  "Takes a `uber/graph` and returns an `uber/graph` that is its minimum spanning tree.
  Implementation of Kruskal's algorithm."
  ([graph]
   (minimum-spanning-tree graph :weight))
  ([graph metric]
   (let [disj-set    (ds-from (uber/nodes graph))
         edges       (sorted-edges graph metric)
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
                      edges)))))

(defn algorithm4
  "Algorithm 4 from the paper. Takes an `uber/graph` and returns an `uber/graph`
  representing a placement of relay nodes with the minimum number of connected
  components."
  [graph comm-range budget]
  (let [mst (atom (-> graph
                    complete-graph
                    (weight-forest comm-range)
                    minimum-spanning-tree))]
    (while (> (total-edge-weight @mst :weight)
              budget)
      (swap! mst
             remove-edge
             (max-edge-by @mst :weight)))
    @mst))

;;;;; Alg5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn four-partitions
  [n]
  (for [w     (range (inc n))
        x     (range (inc n))
        y     (range (inc n))
        z     (range (inc n))
        :when (= n
                 (+ w x y z))]
    [w x y z]))


(defn points-in-square
  "Gets the set of all points which are in his square."
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

(defn get-circle
  ([graph src dst]
   (get-circle (node-location graph src) (node-location graph dst)))
  ([src dst]
   [(midpoint src dst) (* (Math/sqrt 3) (lp-distance src dst))]))

(defn clamp
  [lower upper num]
  (match ((juxt (partial < lower)
                (partial > upper))
          num)
         [true  true ] num
         [false _    ] lower
         [_     false] upper))

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
  ([k center diameter]
   (let [[tl br] (first (cell-locations center diameter diameter))]
     [[tl br] (grid-cell-map center
                             diameter
                             (/ diameter k)
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

(defn get-base-potential
  [{:keys [graph k]} [tl br]]
  (let [points    (points-in-square graph tl br)
        m         (count points)
        remaining (- (inc k) m)
        x-i       (- (:x br) (:x tl))]
    (vec (concat [{:potential 0
                   :points    []}]
                 (map
                   #(assoc {:potential x-i}
                           :points
                           (take % points))
                   (range 1 (inc m)))
                 (take remaining
                       (repeat {:potential ##Inf
                                :points    []}))))))

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
  (letfn [(combine [& args]
            (apply (if (sequential? (first args)) concat +)
                   args))]
    (into [{:potential 0
            :points    []}]
          (map (fn [p]
                 (->> p
                   four-partitions
                   (map (fn [partition]
                          (->> (map (partial get potentials) children)
                            (#(map get % partition))
                            (reduce (partial merge-with combine)))))
                   (apply min-key :potential)
                   (#(update % :potential + (math/abs (apply - (map :x parent)))))))
               (range 1 (inc k))))))

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
   (let [[root tree] (grid-cell-map k
                                    center
                                    diameter)]
     (get (minpot state [root] tree {}) root)))
  ([{:keys [graph center diameter k] :as state} current-nodes tree potentials]
   (let [children (reduce (fn [acc node] (into acc (get tree node)))
                          []
                          current-nodes)]
     (if (= 0 (count children))
       (get-base-potentials state current-nodes potentials)
       (->> (minpot state children tree potentials)
         (potentials-from-children state current-nodes tree))))))

(defn k-min-spanning-tree
  [graph k]
  (let [candidates (->> (pmap (fn [[src dst]]
                                (let [[mid diameter] (get-circle graph src dst)
                                      state          {:graph    graph
                                                      :center   mid
                                                      :diameter diameter
                                                      :k        k}]
                                  (-> state
                                    minpot
                                    (get k)
                                    :points
                                    (->> (into #{})
                                      (induced-subgraph graph))
                                    (minimum-spanning-tree :weight))))
                              (combo/combinations (uber/nodes graph) 2))
                     (filter #(pos? (count (uber/edges %)))))]
    (when (pos? (count candidates))
      (apply min-key #(total-edge-weight % :weight) candidates))))

(defn test-alg4
  [num-nodes max-value comm-range budget]
  (as-> (rand-full-graph num-nodes max-value) $
    (algorithm4 $ comm-range budget)
    (uber/pprint $)))

(defn algorithm5
  "Algorithm 5 from the paper. Takes an `uber/graph` and returns an `uber/graph`
  representing the placement of relay nodes with the maximum connected component
  size."
  ([graph comm-range budget]
   (algorithm5 (-> graph
                 complete-graph
                 (weight-forest comm-range))
               comm-range
               budget
               (uber/count-nodes graph)))
  ([graph comm-range budget k]
   (if (<= k 1)
     (println "No such tree could be found! Reached k=1, which has no minimum spanning tree.")
     (let [kmst (k-min-spanning-tree graph k)]
       (if (or (nil? kmst)
               (> (total-edge-weight kmst :weight) budget))
         (recur graph comm-range budget (dec k))
         kmst)))))

;;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn graph-execution-times
  [max-nodes max-coords comm-range budget]
  (let [graph (rand-full-graph max-nodes max-coords)]
    (doall (map (fn [num-nodes]
                  [num-nodes
                   (->> (with-out-str
                          (time
                            (algorithm5
                              (uber/remove-nodes* graph
                                                  (take (- max-nodes num-nodes)
                                                        (uber/nodes graph)))
                              comm-range
                              budget)))
                     (re-find #"[\d.]+"))])
                (range 2 (inc max-nodes))))))

(def cli-options
  "Parse the command line arguments"
  [;; optional file to read in graph from.
   ["-f" "--file FILE" "File containing a graph encoding. Optional, as the graph could be passed in as string using `-g`"
    :parse-fn read-graph
    :validate [validate-graph "The file supplied is not a valid graph!"]]
   ;; optional string graph. TODO does our parse function work here?
   ["-g" "--graph GRAPH" "String containing a graph encoding. Optional, as the graph could be passed in as a file using `-f`"
    :parse-fn make-graph
    :validate [validate-graph "The graph supplied is not valid!"]]
   ;; Required communications range
   ["-c" "--comm-range FLOAT" "Required floating point comm range for Algorithm 4 and Algorithm 5."
    :parse-fn #(Float/parseFloat %)
    :validate [#(> % 0) "Communication range must be greater than zero."]]
   ;; Required budget
   ["-b" "--budget FLOAT" "Required floating point budget for producing sensor placement."
    :parse-fn #(Float/parseFloat %)
    :validate [#(> % 0) "Budget must be greater than zero."]]
   ;; Help
   ["-h" "--help"]])

(defmacro aif
  [cond true-branch false-branch]
  `(let [~'it ~cond]
     (if ~'it
       ~true-branch
       ~false-branch)))

(def alg4-header "\n------------------------------------------------\nAlgorithm 4 (BCRP-MNCC):\n")
(def alg4-failure "Could not find a solution to Alg4 meeting the specified constraints!")
(def alg5-header "\n------------------------------------------------\nAlgorithm 5 (BCRP-MLCC):\n")
(def alg5-failure "Could not find a solution to Alg5 meeting the specified constraints!")

(defn run-algorithm
  [f {:keys [graph comm-range budget]} header failure-msg]
  (println header)
  (let [res (f graph comm-range budget)]
    (if res
      (uber/pprint res)
      (println failure-msg))))

(defn -main
  " Read in graphs and run algorithms. "
  [& args]
  (let [parsed  (parse-opts args cli-options)
        options (:options parsed)
        options (assoc options :graph (first-defined (:graph options)
                                                     (:file options)))]
    (aif (:errors parsed)
      (print "Errors occurred! Could not run!\n" it)
      (if (:graph options)
        (doall (map (partial apply run-algorithm)
                    [[algorithm4 options alg4-header alg4-failure]
                     [algorithm5 options alg5-header alg5-failure]]))
        (println "You must provide a valid graph either via a file or via command line. Enter --help for more details")))
    (shutdown-agents)))
