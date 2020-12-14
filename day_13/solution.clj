(ns day_13.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
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

;; Ripped the chinese remainder theory code off rosetta code
(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese_remainder
  " Main routine to return the chinese remainder "
  [n a]
  (let [prod (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot prod n_i)           ; p = prod / n_i
                        egcd (extended-gcd p n_i)   ; Extended gcd
                        inv_p (second egcd)]        ; Second item is the inverse
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))] ; Replaces the Python for loop to sum
                                                      ; (map vector n a) is same as
                                                      ; Python's version Zip (n, a)
    (mod sum-prod prod)))                             ; Result line

(def n (map first buses-and-offsets))
(def a (map #(* -1 (last %)) buses-and-offsets))

(chinese_remainder n a)
