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

(defn randomly-locate-nodes
  "Get a dictionary mapping each node to an (x,y,z) location."
  [allnodes max-coord]
  (reduce (fn [acc node]
            (assoc acc
                   node {:x (rand-int max-coord)
                         :y (rand-int max-coord)
                         :z (rand-int max-coord)}))
          {}
          allnodes))

(defn lp-distance
  "Takes two number sequences of equal lenght and produces the euclidean distance"
  ([point1 point2]
   (lp-distance point1 point2 2))
  ([point1 point2 pow]
   (reduce (fn [acc [x, y]]
             (+ acc (Math/pow (- x y) 2)))
           0
           (map vector point1 point2))))

(defn rand-full-graph
  "Create fully connected, random graph with random locations for each node and euclidean graph weights"
  [num-nodes max-coord]
  (let [allnodes      (nodes num-nodes)
        node-locs     (randomly-locate-nodes allnodes max-coord)
        located-nodes (map (fn [x] (conj [] x (get node-locs x))) allnodes)]
    (apply uber/graph
           (reduce (fn [prev [src dst]]
                     (conj prev
                           [src dst {:length
                                     (lp-distance (vals (get node-locs src)) (vals (get node-locs dst)))}]))
                   located-nodes
                   (get-edges allnodes)))))

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
       (unidirectional-edges ,,,)
       (sort #(compare (edge-value graph %1) (edge-value graph %2)) ,,,)))

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

(defn k-minimum-spanning-tree
  "Takes a `uber/graph` and an `int` value `k` and returns an `uber/graph` that is the
  minimum spanning tree with k vertices"
  [graph k]
  nil)

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
   (let [kmst (k-minimum-spanning-tree graph k)
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
