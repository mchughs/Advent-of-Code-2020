(ns day_20.solution
  (:require [clojure.string :as s]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as c]
            [clojure.set :as ss]
            [clojure.walk :as w]
            [utils :as u]))

(def input
  (slurp "day_20/input.txt"))

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

(def tiles
  (map parse-tile
    (s/split input #"\n\n")))

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

;; Part 2 lifted from Fred Overflow

(defn border [^String tile ^long start ^long end ^long step]
  (s/join
    (map #(.charAt tile %)
      (range start end step))))

(defn parse-tiles [^String input]
  (for [[_ id ^String raw] (re-seq #"Tile (\d+):\n([.#\n]{109})" input)]
    {:id     (Integer/parseInt id)
     :top    (border raw 0  10  1)
     :bottom (border raw 99 109 1)
     :left   (border raw 0  110 11)
     :right  (border raw 9  119 11)
     :payload (s/join
                (for [y (range 11 99 11)]
                  (.substring raw (+ y 1) (+ y 9))))}))

(defn rotate
  "Clockwise"
  [{:keys [id top bottom left right ^String payload]}]
  {:id id
   :top right
   :right (s/reverse bottom)
   :bottom left
   :left (s/reverse top)
   :payload (s/join
              (for [x (range 7 -1 -1)
                    y (range 0 64 8)]
                (.charAt payload (+ y x))))})

(defn flip
  [{:keys [id top bottom left right ^String payload]}]
  {:id id
   :top bottom
   :bottom top
   :right (s/reverse right)
   :left (s/reverse left)
   :payload (s/join
              (for [y (range 56 -8 -8)]
                (.substring payload y (+ y 8))))})

(defn symmetries [a]
  (let [b (rotate a)
        c (rotate b)
        d (rotate c)
        e (flip a)
        f (flip b)
        g (flip c)
        h (flip d)]
    [a b c d e f g h]))

(defn find-neighbor
  "Returns the single matching tile which lines up with
   a particular tile's edge (origin) in the given direction."
  [origin direction opposite tiles]
  (let [[neighbor]
        (filter (fn [tile]
                  (and
                    (= (direction origin) (opposite tile))
                    (not= (:id origin) (:id tile))))
          tiles)]
    neighbor))

(defn find-edge
  "Walk to edge in a certain direction."
  [origin direction opposite tiles]
  (let [neighbor (find-neighbor origin direction opposite tiles)]
    (if neighbor
      (recur neighbor direction opposite tiles)
      origin)))

;---

(defn assemble-grid [tiles]
  (let [[somewhere] tiles
        some-top (find-edge somewhere :top  :bottom tiles)
        top-left (find-edge some-top  :left :right tiles)
        left-edge (take-while some?
                    (iterate #(find-neighbor % :bottom :top tiles) top-left))]
    (for [left left-edge]
      (for [tile (take-while some?
                   (iterate #(find-neighbor % :right :left tiles) left))]
        tile))))

(defn assemble-image [grid]
  (s/join
    (for [line grid
          y (range 0 64 8)
          tile line]
      (-> tile :payload (.substring y (+ y 8))))))

(def monster
  (-> "
   (. . . . . . . . . . . . . . . . . .)#(. .{76}
   )#(. . . .)# #(. . . .)# #(. . . .)# # #(.{76})
   (.)#(. .)#(. .)#(. .)#(. .)#(. .)#(. . . .{76})
   "
   (s/replace #"\s" "")
   re-pattern))

(def submerge "$1.$2.$3..$4..$5...$6$7.$8.$9.$10.$11.$12.$13")

(for [tiles (->> input
              parse-tiles
              (mapcat symmetries)
              (iterate next)
              (take 8))]
  (->> tiles
    assemble-grid
    assemble-image
    (#(s/replace % monster submerge))
    (#(s/replace % monster submerge))
    (filter #(= \# %))
    count))
