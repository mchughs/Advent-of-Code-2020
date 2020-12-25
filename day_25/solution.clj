(ns day_25.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as c]
            [clojure.set :as ss]
            [clojure.walk :as w]
            [utils :as u]))

(def input
  (->> "day_25/input.txt"
    slurp
    s/split-lines))

(def card-key (Long. (first input)))
(def door-key (Long. (last input)))
(def divisor 20201227)
(def default-subject-number 7)

(defn transform [subject-number n]
  (mod (* n subject-number) divisor))

(defn public-key->loop-size [public-key subject-number]
  (let [transform* (partial transform subject-number)]
    (loop [ls 0
           n 1]
      (if (= n public-key)
        ls
        (recur
          (inc ls)
          (transform* n))))))

(def card-loop (public-key->loop-size card-key default-subject-number))
(def door-loop (public-key->loop-size door-key default-subject-number))

(=
  (reduce
    (fn [acc _]
      (transform door-key acc))
    1
    (range card-loop))
  (reduce
    (fn [acc _]
      (transform card-key acc))
    1
    (range door-loop)))
