(ns day_19.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as c]
            [clojure.set :as ss]
            [clojure.walk :as w]
            [utils :as u]))

(def input
  (-> "day_19/input.txt"
    slurp))

(defn parse-subrules [input]
  (for [subrules (s/split input #"\|")]
    (for [index (re-seq #"\d+" subrules)]
      (Integer/parseInt index))))

(defn parse-rules [input]
  (for [[_ index literal subrules] (re-seq #"(\d+): (?:\"(\w)\"|(.*))" input)]
    {:left (Integer/parseInt index)
     :right (if (some? literal)
              (first literal)
              (parse-subrules subrules))}))

(defn parse-messages [input]
  (re-seq #"\w+" input))

(defn parse-input [input]
  (let [[rules messages] (s/split input #"\n *\n")]
    {:rules (->> rules
              parse-rules
              (sort-by :left)
              (mapv :right))
     :messages (parse-messages messages)}))

(def parsed (parse-input input))

(defn consume [messages rule rules]
  (cond
    (char? rule)
    (for [message messages
          :when (= rule (first message))]
      (rest message))

    (sequential? rule)
    (for [message messages
          subrules rule
          leftover (reduce
                     (fn [submessages subrule]
                       (consume submessages (rules subrule) rules))
                     (list message)
                     subrules)]
      leftover)))

(defn check-messages [input]
  (let [{:keys [rules messages]} input]
    (count
      (for [message messages
            leftover (consume (list message) (rules 0) rules)
            :when (.isEmpty leftover)]
        message))))

(time (check-messages parsed))
;---

(defn new-rules [rules]
  (assoc rules
    8  '((42)    (42 8))
    11 '((42 31) (42 11 31))))

(defn check-messages* [input]
  (let [{:keys [rules messages]} input]
    (count
      (for [message messages
            leftover (consume (list message) (rules 0) (new-rules rules))
            :when (.isEmpty leftover)]
        message))))

(time (check-messages* parsed))
