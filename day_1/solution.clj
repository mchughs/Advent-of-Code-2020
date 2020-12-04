(ns day_1.solution
  (:require [clojure.math.combinatorics :as c]
            [utils :as u]))

(def input
  (->> "day_1/input.txt"
       slurp
       clojure.string/split-lines
       (map #(Integer. %))))

(defn find-triple [int-list]
  (let [pairs    (c/combinations int-list 2)
        pair-sum (->> pairs
                   (map (fn [[x y]]
                          (list (+ x y)
                                [x y])))
                   (filter (fn [[sum _]]
                             (< sum 2020))))
        new-list (concat pair-sum int-list)
        triples  (c/cartesian-product new-list int-list)
        triple*  (u/find-first (fn [[pair-sum z]]
                                 (let [[sum pair] pair-sum]
                                   (= 2020 (+ sum z))))
                               triples)
        [[_ [x y]] z] triple*]
    [x y z]))

(time (find-triple input))
