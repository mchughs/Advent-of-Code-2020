(ns day_7.solution
  (:require [clojure.string :as s]
            [clojure.set :as ss]
            [utils :as u]))

(def input
  (-> "day_7/input.txt"
      slurp
      s/split-lines))

(def parsed-rules
  (->> input
       (map
         (fn [rule-statement]
           (let [[_ parent-color rules] (re-find #"(\w+ \w+) bags contain(.*)" rule-statement)
                 rules-vec (s/split rules #",")
                 rules-maps (map #(let [[_ number child-color] (re-find #"(?:(?: (\d+) (\w+ \w+) bags?.?)| no other bags\.)" %)]
                                    (if (and number child-color)
                                      {child-color (Integer. number)}
                                      {}))
                              rules-vec)
                 rules-map  (reduce merge rules-maps)]
             {parent-color rules-map})))
       (reduce merge)))

(def simplified-rules
  (reduce-kv (fn [m parent-color rules-map]
               (let [child-colors (set (keys rules-map))]
                 (assoc m parent-color child-colors)))
    {}
    parsed-rules))

(def my-bag "shiny gold")

(defn child-bags->parent-bags [bag-set]
  (->> simplified-rules
       (filter (fn [[parent-color child-colors]]
                 (some (fn [bag]
                         (contains? child-colors bag))
                       bag-set)))
       keys
       set))

(defn find-all-parents [bag]
  (let [parent-bags* (->> #{bag}
                          (iterate child-bags->parent-bags)
                          (take-while #(not (empty? %)))
                          (reduce ss/union))]
    (ss/difference parent-bags* #{bag})))

^{:meta "part1"}
(-> my-bag find-all-parents count)

;---

(defn n-bags-in [rules bag]
  (let [children (get rules bag)]
    (if (empty? children)
      0
      (reduce (fn [acc [child-bag amount]]
                (+ acc
                   amount
                   (* amount
                      (n-bags-in rules child-bag))))
              0
              children))))

^{:meta "part2"}
(n-bags-in parsed-rules my-bag)
