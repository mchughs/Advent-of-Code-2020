(ns day_9.solution
  (:require [clojure.string :as s]
            [clojure.set :as ss]
            [utils :as u]))

(def input
  (->> "day_9/input.txt"
      slurp
      s/split-lines
      (map #(Long. %))
      vec))

(def preamble-length 25)

(defn validate [index target-sum]
  (let [preamble-window (subvec input
                                index
                                (min (+ index preamble-length) (count input)))]
    (for [x preamble-window
          y preamble-window
          :let [sum (+ x y)]
          :when (< x y)
          :when (= target-sum sum)]
      [sum [x y]])))

(def last-satisfying-n
  (last
    (map (comp first first)
      (take-while
        (comp not empty?)
        (map-indexed validate (drop preamble-length input))))))

(def first-invalid-n-loc
  (inc (.indexOf input last-satisfying-n)))

^{:meta "part1"}
(def first-invalid-n
  (nth input first-invalid-n-loc))

;---

^{:meta "part2"}
(first
  (for [index (range 1 first-invalid-n-loc)
        straight-length (range (- first-invalid-n-loc index))
        :let [straight (subvec input index (+ index straight-length))]
        :when (not (empty? straight))
        :let [min-val (apply min straight)
              max-val (apply max straight)]
        :when (= first-invalid-n
                 (reduce + straight))]
    (+ min-val max-val)))
