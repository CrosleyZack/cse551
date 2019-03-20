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
  "Takes an `int` number of nodes (or uses a default 26) and returns the sequence of node names."
  ([]
   (nodes 26))
  ([n]
   (map (comp keyword str char)
        (range 65 (+ 65 n)))))

(defn get-edges
  "Retrieves the edges that exist in a fully connected graph with the set of nodes.
  Removes self referencial nodes"
  [nodes]
  (filter (fn [[src dst]] (not (= src dst)))
          (apply combo/cartesian-product
                 (repeat 2 nodes))))

;; Creates a random, fully connected graph with `int` num-nodes nodes and `int` max-weight per edge.
(defn rand-full-graph
  [num-nodes max-weight]
  (let [allnodes (nodes num-nodes)]
    (apply uber/graph
           (reduce (fn [prev [src dst]]
                     (conj prev
                           [src dst (rand-int max-weight)]))
                   '()
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
    val))

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
  (not= (ds-get-canonical disj-set-atom u)
        (ds-get-canonical disj-set-atom v)))

(defmacro with-disj-set
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

(defn unique-edges
  "Gets the unique edges in the graph (removes bidirectional edges)"
  [graph]
  (filter (fn [x] (= false (:mirror? x)))
          (uber/edges graph)))

(defn edge-weight
  "Takes a `dictionary` edge and produces a `vector` output of source and dest node names."
  [graph edge]
  (:weight (uber/attrs graph [(:src edge) (:dest edge)])))

(defn sorted-edges
  "Given a graph, returns it edges sorted in increasing order."
  [graph]
  (->> graph
    (unique-edges ,,,)
    (sort #(compare (edge-weight graph %1) (edge-weight graph %2)) ,,,)))

(defn minimum-spanning-tree
  "Takes a `uber/graph` and returns an `uber/graph` that is its minimum spanning tree.
  Implementation of Kruskal's algorithm."
  [graph]
  (let [disj-set (ds-from (uber/nodes graph))
        edges    (sorted-edges graph)]
    (apply uber/graph (reduce (fn [acc {:keys [src dest]
                                  :as   edge}]
                          (with-disj-set disj-set
                            (if (ds-shared-root? src dest)
                              (do
                                (ds-union src dest)
                                (conj acc [src dest (edge-weight graph edge)]))
                              acc)))
                        '()
                        edges))))


;;; IO Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-graph
  "Takes a `str` file location and returns the `uber/graph` specified by the file."
  [location]
  nil)

;;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn algorithm4
  "Algorithm 4 from the paper. Takes an `uber/graph` and returns ... something useful"
  [graph]
  (let [mst (minimum-spanning-tree graph)]
    ))

(defn -main
  " Read in graphs and run algorithms. "
  [& args]
  (println "Hello, World!"))
