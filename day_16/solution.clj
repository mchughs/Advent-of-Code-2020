(ns day_16.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as ss]
            [utils :as u]))

(def input
  (-> "day_16/input.txt"
    slurp
    (s/split #"\n\n")))

(def field-ranges
  (->> (nth input 0)
    (re-seq #"\d+-\d+")
    (map #(let [[start end] (re-seq #"\d+" %)]
            [(Long. start) (Long. end)]))
    (sort-by (partial min-key first))))

(def my-ticket
  (->> (nth input 1)
    (re-seq #"\d+")
    (map #(Long. %))))

(def nearby-tickets
  (->> (nth input 2)
    s/split-lines
    (map (fn [ticket-str]
           (->> ticket-str
             (re-seq #"\d+")
             (map #(Long. %)))))
    rest))

(def valid-ranges
  (reduce
    (fn [acc field-ranges]
      (let [[start-acc end-acc] acc
            [start end] field-ranges]
        (when (< end-acc start)
          (prn "No overlap" end-acc))
        [(min start-acc start)
         (max end-acc end)]))
    field-ranges))

^{:meta "part1"}
(def invalid-value-sum
  (let [[min-v max-v] valid-ranges]
    (reduce
      (fn [acc field-values]
        ;; looks like there is only one invalid value per ticket
        (let [invalid-value (remove #(<= min-v % max-v) field-values)]
          (if-not (empty? invalid-value)
            (+ acc (first invalid-value))
            acc)))
      0
      nearby-tickets)))

;---

(def valid-tickets
  (let [[min-v max-v] valid-ranges]
    (remove
      (fn [field-values]
        (some #(not (<= min-v % max-v))
              field-values))
      nearby-tickets)))

(def field-ranges*
  (->> (nth input 0)
    s/split-lines
    (map #(re-matches #"([\w ]+): (\d+-\d+) or (\d+-\d+)" %))

    (map (fn [[_ key-str & ranges]]
           (let [k (s/replace key-str #" " "-")]
             {(keyword k)
              (->> ranges
                (map (fn [my-range]
                       (->> my-range
                         (re-seq #"\d+")
                         (map #(Long. %))))))})))
    (remove nil?)
    (reduce merge)))

(def ticket-size
  (count (first valid-tickets)))

(def field-possibilities
  (reduce-kv
    (fn [m field [r1 r2]]
      (let [[min1 max1] r1
            [min2 max2] r2]
        (merge m
          (->> ticket-size
            range
            (reduce
              #(if (every?
                       (fn [ticket]
                         (let [v (nth ticket %2)]
                           (or (<= min1 v max1)
                               (<= min2 v max2))))
                       valid-tickets)
                (update %1 field conj %2)
                %1)
              {})))))
    {}
    field-ranges*))

(defn assign-field
  [[current-assignments
    remaining-to-assign]]
  (let [[field-key index*]
        (->> remaining-to-assign
          (u/find-first
            (fn [[k v]]
              (= 1 (count v)))))
        index (first index*)]
    [(assoc current-assignments field-key index)
     (->> (dissoc remaining-to-assign field-key)
          (map (fn [[k v]] [k (remove #(=  % index) v)]))
          (into {}))]))

(def field->index
  (->> [{} field-possibilities]
    (iterate assign-field)
    (take 21)
    last
    first))

(def index->field (ss/map-invert field->index))

^{:meta "part2"}
(->> my-ticket
  (map-indexed
    (fn [index v]
      [(index->field index) v]))
  (into {})
  (reduce-kv
    (fn [acc k v]
      (if (s/includes? (name k) "departure")
        (* acc v)
        acc))
    1))
