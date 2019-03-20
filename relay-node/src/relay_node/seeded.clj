(ns relay-node.seeded
  (:refer-clojure :exclude [rand rand-int rand-nth shuffle])
  (:require [clojure.math.numeric-tower :as math]))

;; https://stackoverflow.com/a/45024962
(letfn [(f [randomizer] (lazy-seq (cons (.nextDouble randomizer) (f randomizer))))]
  (defn rand-seq
    [seed]
    (f (java.util.Random. seed))))

(defn make-rand-popper
  [rand-seq]
  (let [head (atom rand-seq)]
    (fn []
      (let [[x & xs] @head]
        (reset! head xs)
        x))))

(def ^:dynamic *pop-rand* (atom (constantly 0)))

(defn rand
  ([] (@*pop-rand*))
  ([n] (* n (@*pop-rand*))))

(defn rand-int
  [n]
  (int (rand n)))

(defn rand-nth
  [coll]
  (nth coll (rand-int (count coll))))

(defn shuffle
  [coll]
  (loop [acc coll
         idx (dec (count coll))]
    (if (= 0 idx)
      acc
      (let [j (rand-int idx)]
        (recur (assoc acc
                      idx (get acc j)
                      j (get acc idx))
               (dec idx))))))

(defn gen-seed
  []
  (long (clojure.core/rand Long/MAX_VALUE)))

(defn seed!
  [seed]
  (reset! *pop-rand* (make-rand-popper (rand-seq seed))))
