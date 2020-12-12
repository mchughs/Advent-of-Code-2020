(ns day_12.solution
  (:require [clojure.string :as s]
            [clojure.set :as ss]))


(def instructions
  (->> "day_12/input.txt"
    slurp
    s/split-lines
    (map #(let [[first & rest] %]
            {:type first
             :value (Integer. (s/join rest))}))))

instructions

;; Define N +1 y and E +1 x and o 0 is E
(def mappings
  {\N (fn [value position]
        (update position :y + value))
   \S (fn [value position]
        (update position :y - value))
   \E (fn [value position]
        (update position :x + value))
   \W (fn [value position]
        (update position :x - value))
   \L (fn [value position]
        (update position :o #(mod (+ % value) 360)))
   \R (fn [value position]
        (update position :o #(mod (- % value) 360)))
   \F (fn [value position]
        (let [[op k] (case (:o position)
                       0   [+ :x]
                       90  [+ :y]
                       180 [- :x]
                       270 [- :y])]
          (update position k op value)))})

(defn apply-instruction
  [{:keys [type value]} position]
  ((get mappings type) value position))

(defn run-instructions [full-instructions start-pos]
  (loop [instructions full-instructions
         position start-pos]
    (if-let [instruction (first instructions)]
      (recur (rest instructions)
             (apply-instruction instruction position))
      position)))

(defn calc-pair-diff
  [{x1 :x y1 :y}
   {x2 :x y2 :y}]
  [(- x2 x1)
   (- y2 y1)])

(defn calc-manhattan
  [p1 p2]
  (->> (calc-pair-diff p1 p2)
       (map #(Math/abs %))
       (reduce +)))

(def start {:x 0 :y 0 :o 0})
(def end (run-instructions instructions start))

^{:meta "part1"}
(calc-manhattan start end)

;---

(def cos
  {0   1
   90  0
   180 -1
   270 0})

(def sin
  {0   0
   90  1
   180 0
   270 -1})

(defn rotate [angle {:keys [x y]}]
  {:x (- (* x (cos angle))
         (* y (sin angle)))
   :y (+ (* x (sin angle))
         (* y (cos angle)))})

(defn center-on-origin
  [{planetX :x planetY :y}
   {satelliteX :x satelliteY :y}]
  {:x (- satelliteX planetX)
   :y (- satelliteY planetY)})

(defn center-on-point
  [{planetX :x planetY :y}
   {satelliteX :x satelliteY :y}]
  {:x (+ satelliteX planetX)
   :y (+ satelliteY planetY)})

(defn apply-rotation
  [angle position waypoint]
  (->> waypoint
       (center-on-origin position)
       (rotate angle)
       (center-on-point position)))

(def mappings*
  {\N (fn [value position waypoint]
        {:position position
         :waypoint (update waypoint :y + value)})
   \S (fn [value position waypoint]
        {:position position
         :waypoint (update waypoint :y - value)})
   \E (fn [value position waypoint]
        {:position position
         :waypoint (update waypoint :x + value)})
   \W (fn [value position waypoint]
        {:position position
         :waypoint (update waypoint :x - value)})
   \L (fn [value position waypoint]
        {:position position
         :waypoint (apply-rotation value position waypoint)})
   \R (fn [value position waypoint]
        {:position position
         :waypoint (apply-rotation (mod (* -1 value) 360) position waypoint)})
   \F (fn [value position waypoint]
        (let [[dx dy] (map (partial * value) (calc-pair-diff position waypoint))
              position' (-> position
                          (update :x + dx)
                          (update :y + dy))
              waypoint' (-> waypoint
                          (update :x + dx)
                          (update :y + dy))]
          {:position position'
           :waypoint waypoint'}))})

(defn apply-instruction*
  [{:keys [type value]} position waypoint]
  ((get mappings* type) value position waypoint))

(defn run-instructions* [full-instructions start-pos start-waypoint]
  (loop [instructions full-instructions
         position start-pos
         waypoint start-waypoint]
    (if-let [instruction (first instructions)]
      (let [{new-position :position
             new-waypoint :waypoint} (apply-instruction* instruction position waypoint)]
        (recur (rest instructions)
               new-position
               new-waypoint))
      position)))

(def start-pos {:x 0 :y 0})
(def start-waypoint {:x 10 :y 1})
(def end-pos (run-instructions* instructions start-pos start-waypoint))

^{:meta "part2"}
(calc-manhattan start-pos end-pos)
