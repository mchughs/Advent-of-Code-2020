(ns day_22.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as c]
            [clojure.set :as ss]
            [clojure.walk :as w]
            [utils :as u]))

(def input
  (-> "day_22/input.txt"
    slurp
    (s/split #"\n\n")))

(def decks
  (->> input
    (map (fn [deck]
           (map #(Long. (last %))
                (re-seq #"\n(\d+)" deck))))))

(def combat-winning-deck
  (loop [d1 (first decks)
         d2 (last decks)]
    (if (or (empty? d1) (empty? d2))
      [d1 d2]
      (let [c1 (first d1)
            c2 (first d2)]
        (if (> c1 c2)
          (recur
            (concat (rest d1) (list c1 c2))
            (rest d2))
          (recur
            (rest d1)
            (concat (rest d2) (list c2 c1))))))))

(defn calculate-score [winning-deck]
  (->> winning-deck
    (u/find-first seq)
    reverse
    (map-indexed (fn [i n] [(inc i) n]))
    (reduce
      (fn [acc [i n]]
        (+ acc
          (* i n)))
      0)))

; ^{:meta "part1"}
(calculate-score combat-winning-deck)

;---

;; part 2 solution lifted rjray

(defn- score-game [game]
  (let [deck (:winner game)]
    (reduce + (map * (reverse deck) (iterate inc 1)))))

(defn- play-game-rec [game]
  (let [p1 (:p1 game)
        p2 (:p2 game)]
    (loop [p1 p1, p2 p2, seen #{}]
      (if (seen (list p1 p2))
        {:winner :p1, :p1 p1}
        (let [[n1 & p1'] p1, [n2 & p2'] p2]
          (cond
            (nil? n1) {:winner :p2, :p2 p2}
            (nil? n2) {:winner :p1, :p1 p1}
            (and (<= n1 (count p1'))
                 (<= n2 (count p2')))
            (let [game' (play-game-rec {:p1 (take n1 p1')
                                        :p2 (take n2 p2')})]
              (case (:winner game')
                :p1 (recur (concat p1'
                                   (list n1 n2))
                           p2'
                           (conj seen (list p1 p2)))
                :p2 (recur p1'
                           (concat p2'
                                   (list n2 n1))
                           (conj seen (list p1 p2)))))
            (< n1 n2) (recur p1'
                             (concat p2'
                                     (list n2 n1))
                             (conj seen (list p1 p2)))
            :else     (recur (concat p1'
                                     (list n1 n2))
                             p2'
                             (conj seen (list p1 p2)))))))))

(defn- score-game-rec [game]
  (score-game {:winner (game (:winner game))}))

(defn- parse-data [[p1 p2]]
  (let [[_ & lines1] (s/split-lines p1)
        [_ & lines2] (s/split-lines p2)]
    {:p1 (map read-string lines1)
     :p2 (map read-string lines2)}))

(defn part-2
  "Day 22 Part 2"
  [input]
  (-> input
      parse-data
      play-game-rec
      score-game-rec))

; (part-2 input)
