(ns day_19.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as c]
            [clojure.set :as ss]
            [clojure.walk :as w]
            [utils :as u]))

(def input
  (-> "day_19/input.txt"
    slurp
    (s/split #"\n\n")))

(def rules
  (->> input
    first
    s/split-lines
    (map #(let [label (Long. (last (re-find #"(\d+):" %)))
                content (last (re-find #": \"*([\d \|\w]+)\"*" %))]
            {:label label :content content}))
    (sort-by :label)
    (map :content)))

(defn apply-sub [z]
  (let [z' (s/split z #"\|")]
    (map
      (fn [s]
        (or (re-matches #"[ab]" s)
            (->> s
                 (re-seq #"\d+")
                 (map #(apply-sub (nth rules (Integer. %)))))))
                 ; (apply concat))))
      z')))

(def x
  (->> rules
       first
       apply-sub
       pr-str))

(-> x
  (s/replace #"\"" "")
  (s/replace #" " "")
  (s/replace #"^\(+" "")
  (s/replace #"\)+$" "")
  (s/replace #"\)\(" "")
  (s/replace #"^[\)]\)\(^[\(]" "|"))
  ; re-pattern)
  ; (s/replace #"(\)\))+(\(\()+" "|"))


  ; ((a((aa)(bb)(ab)(ba))((ab)(ba)(aa)(bb))b))
  ; ^a((aa|bb)|(ab|ba))((ab|ba)|(aa|bb))b$)

  ; (apply c/cartesian-product)
  ; (map flatten)
  ; (map (partial apply str)))
