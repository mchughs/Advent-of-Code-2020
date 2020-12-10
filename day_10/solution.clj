(ns day_10.solution
  (:require [clojure.string :as s]
            [clojure.set :as ss]))

(def adapters
  (->> "day_10/input.txt"
    slurp
    s/split-lines
    (map #(Integer. %))
    sort))

(def jolt-chain
  (let [sorted-vec (-> adapters
                       (conj 0)
                       vec)]
    (as-> sorted-vec $
      (conj $ (+ 3 (last sorted-vec)))
      (into '() $)
      (reverse $))))

(def jolt-diffs
  (reduce
    (fn [acc pair]
      (let [diff (apply - pair)]
        (update acc diff inc)))
    {-1 0
     -2 0
     -3 0}
    (partition 2 1 jolt-chain)))

^{:meta "part1"}
(* (get jolt-diffs -1)
   (get jolt-diffs -3))

; ---

;; Works okay for smaller lists but gets big fast
(defn number-distinct-paths [goal chain]
  (if (= goal (first chain))
    1
    (let [[female & males] (take 4 chain)
          xf (map-indexed
               (fn [index male]
                 (let [diff (- male female)]
                   (if (#{1 2 3} diff)
                     (number-distinct-paths goal (drop (inc index) chain))
                     0))))]
      (transduce xf + males))))

; (number-distinct-paths jolt-chain)

(def islands
  (->> jolt-chain
    (partition 2 1)
    (partition-by #(apply - %))
    (map #(dedupe (apply concat %)))))

(reduce * (map #(number-distinct-paths (last %) %)
               islands))
