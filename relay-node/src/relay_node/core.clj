(ns relay-node.core
  (:gen-class)
  (:require [ubergraph.core :as uber]
            [clojure.math.combinatorics :as combo]))

;;; Non-domain utility functions.

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

;; Creates a random, fully connected graph with `int` num-nodes nodes and `int` max-weight per edge.
(defn rand-full-graph
  [num-nodes max-weight]
  (let [allnodes (nodes num-nodes)]
    (apply uber/graph
           (reduce (fn [prev [src dst]]
                     (conj prev
                           [src dst (rand-int max-weight)]))
                   '()
                   (apply combo/cartesian-product
                          (repeat 2 allnodes))))))

;;; Utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn unique-edges
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
  (let [main-set #{}
        sets     (zipmap (uber/nodes graph) (repeat #{}))
        edges    (sorted-edges graph)]
    (reduce (fn [[main-s node-sets] edge]
              (let [src     (:src edge)
                    dst     (:dest edge)
                    src-set (get node-sets src)
                    dst-set (get node-sets dst)
                    union   (clojure.set/union src-set dst-set)]
                (print-lines
                  []
                  ["main set : " main-s]
                  ["sets for each node : " node-sets]
                  ["edge : " edge]
                  []
                  ["Source : " src]
                  ["Dest : " dst]
                  ["Source Set : " src-set]
                  ["Dest Set : " dst-set]
                  ["Set Union : " union])))
            [main-set sets]
            edges)))

;;; IO Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-graph
  "Takes a `str` file location and returns the `uber/graph` specified by the file."
  [location]
  nil)

(defn -main
  " Read in graphs and run algorithms. "
  [& args]
  (println "Hello, World!"))
