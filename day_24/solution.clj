(ns day_24.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as c]
            [clojure.set :as ss]
            [clojure.walk :as w]
            [utils :as u]))

(def input
  (->> "day_24/input.txt"
    slurp
    s/split-lines))

(def directions
  {"e"  (fn [[x y]] [(+ x 2) y])
   "se" (fn [[x y]] [(+ x 1) (- y 2)])
   "sw" (fn [[x y]] [(- x 1) (- y 2)])
   "w"  (fn [[x y]] [(- x 2) y])
   "ne" (fn [[x y]] [(+ x 1) (+ y 2)])
   "nw" (fn [[x y]] [(- x 1) (+ y 2)])})

(def instructions-set
  (map #(re-seq #"(?<!n|s)e|(?<!n|s)w|se|sw|nw|ne" %)
       input))

(defn apply-instruction [coordinates instruction]
  ((get directions instruction) coordinates))

(def final-destinations
  (map
    #(reduce
       (fn [position instruction]
         (apply-instruction position instruction))
       [0 0]
       %)
    instructions-set))

(def black-tiles
  (->> final-destinations
    frequencies
    (remove (comp even? last))
    (map first)
    set))

^{:meta "part1"}
(count black-tiles)

;---

(defn neighbors [coordinates]
  (map (fn [f] (f coordinates)) (vals directions)))

(defn step [black-hexagons]
  (set
    (for [[loc n] (frequencies (mapcat neighbors black-hexagons))
          :let [black? (black-hexagons loc)]
          :when (or (and black?
                         (< 0 n 3)) ;; stays black
                    (and (not black?)
                         (= n 2)))] ;; flips to black
      loc)))

(defn number-black-tiles-after-n-days [n]
  (->> black-tiles
    (iterate step)
    (take (inc n))
    last
    count))

^{:meta "part2"}
(number-black-tiles-after-n-days 100)
