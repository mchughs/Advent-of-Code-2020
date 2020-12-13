(ns day_13.solution
  (:require [clojure.string :as s]
            [clojure.set :as ss]
            [utils :as u]))

(def input
  (->> "day_13/input.txt"
    slurp
    s/split-lines))

(def desired-dep-time
  (Integer. (first input)))

(def buses
  (->> input
    last
    (re-seq #"\d+")
    (mapv #(Integer. %))))

(defn run-calc []
  (let [[id-index dep-time]
        (->> buses ;; going to assume none of our busses arrive precisely at our desired-dep-time
          (map #(quot desired-dep-time %))
          (map #(* %1 (inc %2)) buses)
          (map-indexed (fn [index v] [index v]))
          (apply min-key second))

        id        (get buses id-index)
        wait-time (- dep-time desired-dep-time)]
    (* id wait-time)))

^{:meta "part1"}
(run-calc)

;---

(def buses-and-offsets
  (as-> input $
    (last $)
    (s/split $ #",")
    (map #(when-not (= "x" %) (Integer. %)) $)
    (map-indexed #(when %2 [%2 %1]) $)
    (remove nil? $)))

(def first-id
  (first (first buses-and-offsets)))

(def multiples-ids
  (->> buses-and-offsets
    (filter
      (fn [[id offset]]
        (zero? (mod offset first-id))))
    (map first)
    set))

(def multiples
  (map #(* % (apply * multiples-ids)) (range)))

(def offset-buses
  (remove #(contains? multiples-ids (first %))
          buses-and-offsets))

(time
  ; (-
     (u/find-first
       (fn [t]
         (every?
           (fn [[id offset]]
             (= first-id
                (+ offset
                   (mod t id))))
           offset-buses))
       (take 1000 multiples)))
     ; first-id))
