(ns day_11.solution
  (:require [clojure.string :as s]
            [clojure.set :as ss]
            [utils :as u]))

(def input
  (->> "day_11/input.txt"
    slurp
    s/split-lines))

(def initial-tiles
  (->> input
    (map-indexed
      (fn [y row]
        (->> row
          (map-indexed
            (fn [x tile]
              [tile [x y]])))))
    (apply concat)))

(defn tiles->seat-map [tiles]
  (reduce (fn [m [k v]]
            (update m
                    (get {\L :empty
                          \# :occupied
                          \. :floor} k)
                    conj
                    v))
          {:empty #{}
           :occupied #{}
           :floor #{}}
          tiles))

(def init-seat-map (tiles->seat-map initial-tiles))

(defn measure-counts [m]
  (reduce-kv
    (fn [m' k v]
      (assoc m' k (count v)))
    {}
    m))

(def dims
  {:x {:min 0
       :max (dec (count (first input)))}
   :y {:min 0
       :max (dec (count input))}})

(defn coords-in-bounds? [x y]
  (and (<= (get-in dims [:x :min]) x (get-in dims [:x :max]))
       (<= (get-in dims [:y :min]) y (get-in dims [:y :max]))))

(defn relative-location [[x y]]
  (cond
    (u/xor (< (get-in dims [:x :min]) x (get-in dims [:x :max]))
           (< (get-in dims [:y :min]) y (get-in dims [:y :max])))
    :edge
    (and (or (= (get-in dims [:x :min]) x)
             (= (get-in dims [:x :max]) x))
         (or (= (get-in dims [:y :min]) y)
             (= (get-in dims [:y :max]) y)))
    :corner
    :else :middle))

(defn neighbors [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= dx dy 0)
        :let [x' (+ dx x)
              y' (+ dy y)]
        :when (coords-in-bounds? x' y')]
    [x' y']))

(defn step [{:keys [empty occupied floor]}]
  (let [not-occupied (ss/union empty floor)
        e->o (->> occupied
                  (mapcat neighbors)
                  set
                  (ss/difference empty))
        o->e (set
               (concat
                 ;; occupied surrounded by occupied
                 (for [[loc n] (frequencies (mapcat neighbors occupied))
                       :let [rel (relative-location loc)]
                       :when (and (occupied loc)
                                  (or (and (= n 8) ;; totally surrounded by occupied
                                           (= :middle rel))
                                      (and (= n 5) ;; totally surrounded by occupied
                                           (= :edge rel))))]
                   loc)
                 ;; occupied surrounded by some not-occupied
                 (for [[loc n] (frequencies (mapcat neighbors not-occupied))
                       :let [rel (relative-location loc)]
                       :when (and (occupied loc)
                                  (or (and (< n 5) ;; has 4 or more occupied neighbors
                                           (= :middle rel))
                                      (and (< n 2) ;; has 4 or more occupied neighbors
                                           (= :edge rel))))]
                   loc)))]
    {:floor floor
     :empty (ss/union o->e
                      (ss/difference empty
                                     e->o))
     :occupied (ss/union e->o
                         (ss/difference occupied
                                        o->e))}))

^{:meta "part1"}
(def steps
  (map measure-counts
       (iterate step init-seat-map)))

;---

(def g-mappings
  {:right   (fn [[x1 y1] [x2 y2]] (if (= x1 (min x1 x2)) [x1 y1] [x2 y2]))
   :left    (fn [[x1 y1] [x2 y2]] (if (= x1 (max x1 x2)) [x1 y1] [x2 y2]))
   :bottom  (fn [[x1 y1] [x2 y2]] (if (= y1 (min y1 y2)) [x1 y1] [x2 y2]))
   :top     (fn [[x1 y1] [x2 y2]] (if (= y1 (max y1 y2)) [x1 y1] [x2 y2]))
   :top-right
   (fn [[x1 y1] [x2 y2]]
     (if (= (Math/abs (- y1 x1))
            (min (Math/abs (- y1 x1))
                 (Math/abs (- y2 x2))))
       [x1 y1]
       [x2 y2]))
   :top-left
   (fn [[x1 y1] [x2 y2]]
     (if (= (+ y1 x1)
            (max (+ y1 x1)
                 (+ y2 x2)))
       [x1 y1]
       [x2 y2]))
   :bottom-right
   (fn [[x1 y1] [x2 y2]]
     (if (= (+ y1 x1)
            (min (+ y1 x1)
                 (+ y2 x2)))
       [x1 y1]
       [x2 y2]))
   :bottom-left
   (fn [[x1 y1] [x2 y2]]
     (if (= (Math/abs (- y1 x1))
            (max (Math/abs (- y1 x1))
                 (Math/abs (- y2 x2))))
       [x1 y1]
       [x2 y2]))})

(defn compare-it [k a b]
  ((k g-mappings) a b))

(defn neighbors* [{:keys [empty occupied floor]} [x y]]
  (let [assign-fn (fn [coords]
                    (cond
                      (empty coords)
                      :empty
                      (occupied coords)
                      :occupied))
        possibles (for [dx (range (* -1 (get-in dims [:x :max]))
                                  (inc  (get-in dims [:x :max])))
                        dy (range (* -1 (get-in dims [:y :max]))
                                  (inc  (get-in dims [:y :max])))
                        :when (and (not= dx dy 0)
                                   (or (= (Math/abs dy)
                                          (Math/abs dx))
                                       (u/xor (zero? dy)
                                              (zero? dx))))
                        :let [x' (+ dx x)
                              y' (+ dy y)]
                        :when (and (not (floor [x' y']))
                                   (coords-in-bounds? x' y'))
                        :let [position (cond
                                        (and (pos? dx)
                                             (zero? dy)) :right
                                        (and (neg? dx)
                                             (zero? dy)) :left
                                        (and (zero? dx)
                                             (pos? dy)) :top
                                        (and (zero? dx)
                                             (neg? dy)) :bottom
                                        (and (pos? dx)
                                             (neg? dy)) :top-right
                                        (and (neg? dx)
                                             (neg? dy)) :top-left
                                        (and (pos? dx)
                                             (pos? dy)) :bottom-right
                                        (and (neg? dx)
                                             (pos? dy)) :bottom-left)]]
                    [position [x' y']])
        counts (->> possibles
                    (reduce
                     (fn [m [k curr]]
                       (assoc m k
                         (if-let [challenger (k m)]
                           (compare-it k curr challenger)
                           curr)))
                     {})
                    (reduce-kv
                      (fn [acc-m dir coords]
                        (update acc-m (assign-fn coords) inc))
                      {:empty 0 :occupied 0}))]
    counts))

(defn step* [{:keys [empty occupied floor] :as seat-map}]
  (let [e->o (->> empty
               (map
                 #(let [{occ :occupied} (neighbors* seat-map %)]
                    (when (zero? occ)
                      %)))
               (remove nil?)
               set)
        o->e (->> occupied
               (map
                 #(let [{occ :occupied} (neighbors* seat-map %)]
                    (when (<= 5 occ)
                      %)))
               (remove nil?)
               set)]
    {:floor floor
     :empty (ss/union o->e
                      (ss/difference empty
                                     e->o))
     :occupied (ss/union e->o
                         (ss/difference occupied
                                        o->e))}))

(time (take 2 (map measure-counts (iterate step* init-seat-map))))
