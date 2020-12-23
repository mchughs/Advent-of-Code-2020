(ns day_23.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as c]
            [clojure.set :as ss]
            [clojure.walk :as w]
            [utils :as u]))

(def input
  (->> "day_23/input.txt"
    slurp
    s/trim
    (mapv #(Long. (str %)))))

(def arranged-options
  (-> (range 1 10)
    reverse
    cycle))

(defn center-on [cups index curr]
  (let [i (.indexOf cups curr)
        diff (- i index)]
    (if (zero? diff)
      cups
      (->> cups
        cycle
        (drop diff)
        (take 9)))))

(defn run-move [{:keys [move-n cups]}]
  (let [index (mod (dec move-n) 9)
        extended-cups (vec (concat cups cups))
        curr (get cups index)
        sub-cups (subvec extended-cups (inc index) (+ 3 (inc index)))
        removed (set (conj sub-cups curr))
        remaining (remove #(contains? removed %) cups)
        dest (->> arranged-options
               (drop (inc (- 9 curr)))
               (take 9)
               (u/find-first #(not= -1 (.indexOf remaining %))))
        a (->> cups
            (take (inc index))
            (drop (max 0 (- (inc index) 6))))
        b (drop (+ 3 (inc index)) cups)
        ab (flatten [a b])
        [a' b'] (split-at (inc (.indexOf ab dest)) ab)
        new-cups (-> [a' sub-cups b']
                   flatten
                   (center-on index curr)
                   vec)]
    {:move-n (inc move-n)
     :cups new-cups}))

(defn run-x-moves [init-cups x-moves]
  (->> {:move-n 1 :cups init-cups}
    (iterate run-move)
    (take (inc x-moves))
    last
    :cups))

^{:meta "part1"}
(let [cups (run-x-moves input 100)
      start (inc (.indexOf cups 1))]
  (->> cups
    cycle
    (drop start)
    (take 8)
    (apply str)))

;---

; Part 2 borrowed from another user. Not my work
(def number-of-moves 10000000)

(def number-of-cups 1000000)

(def cups (concat input (range 10 (inc number-of-cups))))

(defn- make-linked [input]
  (let [cnt    (count input)
        linked (vec (repeat (inc cnt) nil))]
    (assoc (reduce (fn [lnkd [a b]]
                     (assoc lnkd a b))
                   linked (partition 2 1 input))
      (last input) (first input)
      0 (first input))))

(defn- find-destination [val seen]
  (loop [val val]
    (cond
      (seen val) (recur (mod (dec val) (inc number-of-cups)))
      :else      val)))

(defn- dump [state]
  (let [cur  (first state)
        nmap (zipmap (range 1 (count state)) (rest state))
        unseen (set state)]
    (loop [idx cur, unseen unseen, lst ()]
      (case unseen
        #{} (reverse lst)
        (recur (state idx) (disj unseen idx) (cons idx lst))))))

(defn- adjust [state]
  (let [cur  (state 0)
        a    (state cur)
        b    (state a)
        c    (state b)
        n    (state c)
        cval (find-destination (dec cur) #{a b c 0})
        ins  (state cval)]
    (assoc state 0 n, cur n, cval a, c ins)))

(defn- run-game [n start]
  (reduce (fn [state _] (adjust state))
    start
    (range n)))

(defn- get-new-answer [game]
  [(game 1) (game (game 1))])

^{:meta "part2"} ; Part 2 borrowed from another user rjray. Not my work
(->> cups
     make-linked
     (run-game number-of-moves)
     get-new-answer
     (apply *)
     time)
