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
