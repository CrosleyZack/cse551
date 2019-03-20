(ns relay-node.core
  (:gen-class)
  (:require [ubergraph.core :as uber]))

;;; Generate a random graph for testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn nodes
  "Takes an `int` number of nodes (or uses a default 26) and returns the sequence of node names."
  ([]
   (nodes [26]))
  ([n]
   (map keyword
        (map (comp str char)
             (range 65 (+ 65 n))))))

;; Performs cartesian product on n input `sequences`.
;; See https://stackoverflow.com/questions/18246549/cartesian-product-in-clojure.
(defmacro cart [& lists]
  (let [syms (for [_ lists] (gensym))]
    `(for [~@(mapcat list syms lists)]
       (list ~@syms))))

;; Creates a random, fully connected graph with `int` num-nodes nodes and `int` max-weight per edge.
(defmacro rand-full-graph
  [num-nodes max-weight]
  (let [allnodes (nodes num-nodes)]
  `(uber/graph
     ~@(reduce
         (fn [prev [src dst]]
           (conj prev
                 `[~src ~dst ~(rand-int max-weight)}]))
         '()
         (cart allnodes allnodes)))))

;;; Utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn unique-edges
  [graph]
  (filter (fn [x] (= false (:mirror? x))) (uber/edges graph)))

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
  (let [main-set set
        sets (zipmap (uber/nodes graph) (repeat set))
        edges (sorted-edges graph)]
    (reduce (fn [[main-s node-sets] edge]
              (let [src (:src edge)
                    dst (:dest edge)
                    src-set (get node-sets src)
                    dst-set (get node-sets dst)
                    union (clojure.set/union src-set dst-set)]
                (if (not (= src-set dst-set))
                  (conj '() (conj main-s edge) (assoc node-sets src union dst union))
                  (conj '() main-s node-sets))))
            (conj '() main-set sets)
            edges)))

(defn minimum-spanning-tree
  "Takes a `uber/graph` and returns an `uber/graph` that is its minimum spanning tree.
  Implementation of Kruskal's algorithm."
  [graph]
  (let [main-set set
        sets     (zipmap (uber/nodes graph) (repeat set))
        edges    (sorted-edges graph)]
    (reduce (fn [[main-s node-sets] edge]
              (do
                (println)
                (println "Main Set : " main-s)
                (println "Sets for each node : " node-sets)
                (println "Edge : " edge)
                (let [src     (:src edge)
                      dst     (:dest edge)
                      src-set (get node-sets src)
                      dst-set (get node-sets dst)
                      union   (clojure.set/union src-set dst-set)] ; Fails here. Not sure why.
                  (do
                    (println)
                    (println "Source : " src)
                    (println "Dest : " dst)
                    (println "Source Set : " src-set)
                    (println "Dest Set : " dst-set)
                    (println "Set Union : " union)))))
            (conj [] main-set sets)
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
