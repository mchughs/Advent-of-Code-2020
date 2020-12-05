(ns day_5.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as m]))

(def input
  (->> "day_5/input.txt"
       slurp
       s/split-lines
       (map #(drop 1 (re-find #"([FB]{7})([LR]{3})" %)))))

(def char->int
  {\F 0 \B 1
   \L 0 \R 1})

(defn s->int [s]
  (->> s
    reverse
    (map-indexed (fn [index char]
                   (* (m/expt 2 index)
                      (char->int char))))
    (reduce +)))

(defn boarding-pass->id [bp-strings]
  (let [[row col] (map s->int bp-strings)]
    (+ col (* row 8))))

(def ids (map boarding-pass->id input))

^{:meta "part1"}
(apply max ids)

;---

(def sorted-ids (sort ids))

(def id (inc
          (nth sorted-ids
            (first
              (filter (fn [index]
                        (let [current-id (nth sorted-ids index)
                              next-id (nth sorted-ids (inc index))]
                          (not= 1 (- next-id current-id))))
                (range (dec (count sorted-ids))))))))

id
