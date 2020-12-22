(ns day_22.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as c]
            [clojure.set :as ss]
            [clojure.walk :as w]
            [utils :as u]))

(def input
  (-> "day_22/input.txt"
    slurp
    (s/split #"\n\n")))

(def decks
  (->> input
    (map (fn [deck]
           (map #(Long. (last %))
                (re-seq #"\n(\d+)" deck))))))

(def combat-winning-deck
  (loop [d1 (first decks)
         d2 (last decks)]
    (if (or (empty? d1) (empty? d2))
      [d1 d2]
      (let [c1 (first d1)
            c2 (first d2)]
        (if (> c1 c2)
          (recur
            (concat (rest d1) (list c1 c2))
            (rest d2))
          (recur
            (rest d1)
            (concat (rest d2) (list c2 c1))))))))

(defn calculate-score [winning-deck]
  (->> winning-deck
    (u/find-first seq)
    reverse
    (map-indexed (fn [i n] [(inc i) n]))
    (reduce
      (fn [acc [i n]]
        (+ acc
          (* i n)))
      0)))

^{:meta "part1"}
(calculate-score combat-winning-deck)

;---
