(ns day_14.solution
  (:require [clojure.string :as s]
            [clojure.set :as ss]
            [clojure.math.numeric-tower :as m]
            [clojure.pprint :as p]
            [utils :as u]))

(def input
  (-> "day_14/input.txt"
    slurp
    (s/split #"(?=mask)")))

(def my-program
  (->> input
    (map #(let [[_ mask-str] (re-find #"mask = ([10X]+)" %)
                mask (->> mask-str
                          reverse
                          (map-indexed (fn [i bit]
                                         (case bit
                                           \0 [i :clears]
                                           \1 [i :sets]
                                           \X nil)))
                          (remove nil?)
                          (reduce
                            (fn [m [i k]]
                              (update m k conj i))
                            {:sets #{}
                             :clears #{}}))]
               (->> %
                    (re-seq #"mem\[(\d+)\] = (\d+)")
                    (map (fn [[_ address value]]
                           {:address (Integer. address)
                            :value (Integer. value)
                            :mask mask})))))
    (apply concat)
    (reduce
      (fn [m {:keys [address value mask]}]
        (assoc m address [value mask]))
      {})))

(defn run-program [program]
  (reduce-kv
    (fn [acc _ [value {:keys [sets clears]}]]
      (let [masked-value
            (as-> value $
              (reduce bit-set $ sets)
              (reduce bit-clear $ clears))]
        (+ acc masked-value)))
    0
    program))

^{:meta "part1"}
(run-program my-program)

;---

(defn gen-addresses [seed my-sets my-wilds]
  (let [init-address (reduce bit-set seed my-sets)]
    (loop [wilds my-wilds
           addresses (list init-address)]
      (if (empty? wilds)
        addresses
        (let [[wild & remaining] wilds
              new-addresses (->> addresses
                              (map #((juxt bit-set bit-clear) % wild))
                              flatten)]
          (recur remaining new-addresses))))))

(def my-program*
  (->> input
    (map #(let [[_ mask-str] (re-find #"mask = ([10X]+)" %)
                mask (->> mask-str
                          reverse
                          (map-indexed (fn [i bit]
                                         (case bit
                                           \0 nil
                                           \1 [i :sets]
                                           \X [i :wilds])))
                          (remove nil?)
                          (reduce
                            (fn [m [i k]]
                              (update m k conj i))
                            {:sets #{}
                             :wilds #{}}))
                {:keys [sets wilds]} mask]
            (->> %
              (re-seq #"mem\[(\d+)\] = (\d+)")
              (map (fn [[_ address value]]
                     {:addresses (gen-addresses (Integer. address) sets wilds)
                      :value (Integer. value)})))))
    (apply concat)
    (map (fn [{:keys [addresses value]}]
           (reduce
             #(assoc %1 %2 value)
             {}
             addresses)))
    (reduce merge)))

(defn run-program* [program]
  (reduce-kv
    (fn [acc _ value]
      (+ acc value))
    0
    program))

^{:meta "part2"}
(run-program* my-program*)
