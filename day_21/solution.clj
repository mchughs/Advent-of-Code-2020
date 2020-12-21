(ns day_21.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as c]
            [clojure.set :as ss]
            [clojure.walk :as w]
            [utils :as u]))

(def input
  (->> "day_21/input.txt"
    slurp))

(def allergens-map
  (->> input
    s/split-lines
    (map #(s/split % #" \(contains "))
    (map #(let [[ingredients-str allergens-str] %
                allergens (-> allergens-str
                            (s/replace #"," "")
                            (s/replace #"\)" "")
                            (s/split #" "))
                ingredients (s/split ingredients-str #" ")]
            (->> allergens
              (map (fn [allergen]
                     {(keyword allergen) (set ingredients)}))
              (reduce merge {}))))))

(def all-ingredients
  (->> allergens-map
    (apply merge-with ss/union)
    (reduce-kv
      (fn [acc allergen ingredients]
        (ss/union acc ingredients))
      #{})))

(def suspicious-ingredients
  (->> allergens-map
    (apply merge-with ss/intersection)
    (reduce-kv
      (fn [acc allergen ingredients]
        (ss/union acc ingredients))
      #{})))

(def safe-ingredients
  (ss/difference all-ingredients suspicious-ingredients))

^{:meta "part1"}
(reduce
  (fn [acc ingredient]
    (+ acc
      (->> input
          (re-seq (re-pattern (str "(^|\\s)" ingredient "($|\\s)")))
          count)))
  0
  safe-ingredients)

;---

(def allergen->ingredient
  (loop [remaining (apply merge-with ss/intersection allergens-map)
         identified {}]
    (if (empty? remaining)
      identified
      (let [[allergen ingredient']
            (u/find-first (fn [[allergen ingredients]]
                            (= 1 (count ingredients)))
                          remaining)
            ingredient (first ingredient')]

        (recur
          (->> allergen
            (dissoc remaining)
            (u/update-all #(ss/difference % ingredient')))
          (assoc identified allergen ingredient))))))

(->> allergen->ingredient
  (sort-by first)
  (map last)
  (s/join ","))
