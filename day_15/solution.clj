(ns day_15.solution
  (:require [clojure.string :as s]
            [clojure.set :as ss]
            [clojure.math.numeric-tower :as m]
            [clojure.pprint :as p]
            [utils :as u]))

(def input
  (as-> "day_15/input.txt" $
    (slurp $)
    (s/trim-newline $)
    (s/split $ #",")
    (mapv #(Long. %) $)))

(defn next-number [turn-number prev-spoken last-spoken]
  (if (.contains prev-spoken last-spoken)
    (let [last-turn-number (dec turn-number)
          prev-turn-number (inc (.lastIndexOf prev-spoken last-spoken))]
      (- last-turn-number prev-turn-number))
    0))

(defn call-a-number [my-vec]
  (let [prev-spoken (butlast my-vec)
        last-spoken (last my-vec)
        turn-number (inc (count my-vec))
        next-number (next-number turn-number prev-spoken last-spoken)]
    (conj my-vec next-number)))

(def n (inc (- 2020 (count input))))

(defn find-the-xth [x]
  (->> input
    (iterate call-a-number)
    (take x)
    last
    last))

^{:meta "part1"}
(time (find-the-xth n))

;---

(def my-list (into '() input))

(defn next-number* [prev-spoken last-spoken prev-seen]
  (if (contains? prev-seen last-spoken)
    (inc (.indexOf prev-spoken last-spoken))
    0))

(defn call-a-number* [[my-list prev-seen]]
  (let [[last-spoken & prev-spoken] my-list
        next-number (next-number* prev-spoken last-spoken prev-seen)]
    [(conj my-list next-number)
     (conj prev-seen next-number)]))


(defn find-the-xth* [x]
  (->> [my-list (set (rest my-list))]
    (iterate call-a-number*)
    (take x)
    last
    first
    first))

;slightly better part 1
(time (find-the-xth* n))

;---

(def my-list* (into '() input))

(defn call-a-number** [{:keys [my-list record turn] :as state}]
  (let [last-instance (-> my-list first record)
        next-number (if (< 1 (count last-instance))
                      (- (first last-instance)
                         (last last-instance))
                      0)]
    (-> state
      (update :my-list conj next-number)
      (update-in [:record next-number]
                 (fn [turns-witnessed]
                   (let [new (conj turns-witnessed turn)]
                     (if (> (count new) 2)
                       (drop-last new)
                       new))))
      (update :turn inc))))

(def init-record
  (->> input
    (map-indexed (fn [x y] [x y]))
    (reduce (fn [m [i v]] (assoc m v (list (inc i))))
            {})))

(defn find-the-xth** [x]
  (->> {:my-list my-list*
        :record init-record
        :turn (inc (count my-list*))}
    (iterate call-a-number**)
    (take x)
    last
    :my-list
    first))

(def m (inc (- 30000000 (count input))))

^{:meta "part2"}
(time (find-the-xth** m))
