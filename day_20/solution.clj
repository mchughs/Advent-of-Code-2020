(ns day_20.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as c]
            [clojure.set :as ss]
            [clojure.walk :as w]
            [utils :as u]))

(def input
  (-> "day_20/input.txt"
    slurp
    (s/split #"\n\n")))

(defn parse-tile [tile-str]
  (let [id (Long. (re-find #"\d+" tile-str))
        tiles (-> tile-str
                  (s/split #"\:")
                  last
                  s/trim
                  s/split-lines)
        borders [(first tiles)
                 (->> tiles (map last) (apply str))
                 (last tiles)
                 (->> tiles (map first) (apply str))]]
    {:id id
     ;; arranged clockwise: top right bottom left
     :borders borders}))

(def tiles (map parse-tile input))

(defn rotate-counter-clockwise [tile]
  (update tile :borders
    (fn [borders]
      (->> borders
        cycle
        (drop 1)
        (take (count borders))
        (map-indexed #(if (odd? %1)
                        (apply str (reverse %2))
                        %2))))))

(defn count-matching [tile]
  (let [all-tile-borders (->> tiles
                           (remove #(= % tile))
                           (map #(->> %
                                   (iterate rotate-counter-clockwise)
                                   (take 4)
                                   (map :borders)
                                   (apply concat)
                                   set)))]
    (reduce
      (fn [acc border]
          (if (some #(contains? % border) all-tile-borders)
            (inc acc)
            acc))
      0
      (:borders tile))))

^{:meta "part1"}
(->> tiles
  (map count-matching)
  (map-indexed #(when (= %2 2) %1))
  (remove nil?)
  (map #(-> tiles (nth %) :id))
  (reduce * 1))

;---

(defn rotations [borders]
  (->> borders
    (map (comp s/join reverse))
    (concat borders)
    set))

(rotations ["###.#..###" "#######.##" ".##..##..#" "##..###.#."])

(defn find-matches [{:keys [id borders] :as tile}]
  (let [tiles' (->> tiles
                 (remove #(= % tile))
                 (map #(update % :borders rotations)))
        connected (->> borders
                    (map (fn [border]
                           (let [{connected-id :id} (u/find-first
                                                      #(contains? (:borders %) border)
                                                      tiles')]
                              connected-id)))
                    (remove nil?))]
    {id connected}))

(def connection-map
  (->> tiles
    (map find-matches)
    (reduce merge)))

; (defn construct-outer-ring [m]
;   (let [pieces (->> m
;                  (remove #(< 3 (count (last %))))
;                  (reduce (fn [acc [k v]]
;                            (update acc (count v) conj {k v}))
;                    {2 '()
;                     3 '()}))
;         corners (reduce merge (get pieces 2))
;         edges   (reduce merge (get pieces 3))]
;     [corners edges]))

; (construct-outer-ring connection-map)

(def side-length
  (-> tiles count Math/sqrt int))

; (def template
;   (->> nil
;     (repeat side-length)
;     (repeat side-length)))

(defn neighbors [i l]
  (let [indeces (filter #(< -1 % (count tiles))
                  [(inc i)
                   (dec i)
                   (+ i side-length)
                   (- i side-length)])]
    (->> indeces
      (map #(nth l %))
      (remove nil?))))

(loop [options connection-map
       grid (repeat (count tiles) nil)]
  (if-not (some nil? (flatten grid))
    grid
    (let [i (.indexOf nil grid)
          restrictions (neighbors i grid)]
      (if (empty? restrictions)
        (recur (->> options (drop 0) (reduce merge {}))
          (assoc grid i (first options)))

        ())
      (recur
        (dissoc)))))

    ; (let [[i j]]
    ;   body)
    ; (recur (dissoc options))))


; (def first-row
;   (let [first-corner (u/find-first (fn [[k v]] (= 2 (count v))) connection-map)]
;     (loop [m (remove #(= % first-corner) connection-map)
;            row [(first first-corner)]]
;       (if (empty? m)
;         row
;         (let [id (first row)]
;           (u/find-first (fn [[k v]] (and (= 3 (count v))) connection-map)))))))
