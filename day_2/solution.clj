(ns day_2.solution
  (:require [utils :as u]))

(def input
  (->> "day_2/input.txt"
       slurp
       clojure.string/split-lines
       (map #(re-find #"(\d+)-(\d+)\s(\w):\s(\w+)"
                      %))))

(defn password-checker
  ^{:meta "part1"}
  [password-summary]
  (let [[_ min-s max-s letter password] password-summary
        char (-> letter char-array first)
        freq (get (frequencies password) char 0)
        [min max] (map #(Integer. %) [min-s max-s])]
    (<= min freq max)))

(count (filter password-checker input))

;---
(defn password-checker
  ^{:meta "part2"}
  [password-summary]
  (let [[_ start-s end-s letter password] password-summary
        char (-> letter char-array first)
        [start end] (map #(dec (Integer. %))
                         [start-s end-s])
        start? (= char (nth password start))
        end? (= char (nth password end))]
    (u/xor start? end?)))

(count (filter password-checker input))
