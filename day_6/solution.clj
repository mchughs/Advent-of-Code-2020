(ns day_6.solution
  (:require [clojure.string :as s]
            [clojure.set :as ss]))

(def input
  (-> "day_6/input.txt"
      slurp
      (s/split #"\n\n")))

(def groups
  (map #(->> %
          s/split-lines
          (apply concat)
          set)
       input))

^{:meta "part1"}
(->> groups
     (map count)
     (reduce +))

;---

(defn unanimous [group]
  (->> group
       s/split-lines
       (map set)
       (apply ss/intersection)
       count))

^{:meta "part2"}
(->> input
     (map unanimous)
     (reduce +))
