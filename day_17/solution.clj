(ns day_17.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as ss]
            [utils :as u]))

(def input
  (-> "day_17/input.txt"
    slurp
    s/split-lines))

(def init-grid
  (->> input
    (map-indexed
      (fn [y row]
        (->> row
          (map-indexed
            (fn [x ch]
              (when (= \# ch)
                [x y 0])))
          (remove nil?))))
    (apply concat)
    set))

(defn neighbors [[x y z]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        :when (not (= 0 dx dy dz))]
    [(+ dx x) (+ dy y) (+ dz z)]))

(defn step [cells]
  (set
    (for [[loc n] (->> cells
                    (mapcat neighbors)
                    frequencies)
          :let [active? (cells loc)]
          :when (or (= n 3)
                    (and (= n 2)
                         active?))]
      loc)))

(def n-boot-cycles (inc 6))

(defn m-cubes-after-n-cycles [n]
  (->> init-grid
    (iterate step)
    (take n)
    last
    count))

^{:meta "part1"}
(m-cubes-after-n-cycles n-boot-cycles)

;---

(def init-grid*
  (->> input
    (map-indexed
      (fn [y row]
        (->> row
          (map-indexed
            (fn [x ch]
              (when (= \# ch)
                [x y 0 0])))
          (remove nil?))))
    (apply concat)
    set))

(defn neighbors* [[x y z w]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        dw [-1 0 1]
        :when (not (= 0 dx dy dz dw))]
    [(+ dx x) (+ dy y) (+ dz z) (+ dw w)]))

(defn step* [cells]
  (set
    (for [[loc n] (->> cells
                    (mapcat neighbors*)
                    frequencies)
          :let [active? (cells loc)]
          :when (or (= n 3)
                    (and (= n 2)
                         active?))]
      loc)))

(defn m-cubes-after-n-cycles* [n]
  (->> init-grid*
    (iterate step*)
    (take n)
    last
    count))

^{:meta "part2"}
(m-cubes-after-n-cycles* n-boot-cycles)
