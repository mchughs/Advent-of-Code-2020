(ns day_3.solution)

(def input
  (->> "day_3/input.txt"
       slurp
       clojure.string/split-lines))

(def width (count (first input)))
(def depth (count input))

(defn traverse [x-slope y-slope]
  (for [i (range (quot depth y-slope))
        :let [row (nth input (* i y-slope))
              cell (nth row (mod (* i x-slope)
                                 width))]]
    (= cell \#)))

(defn count-trees [[x-slope y-slope]]
  (->> (traverse x-slope y-slope)
       (filter identity)
       count))

(def slopes
  '([1 1] [3 1] [5 1] [7 1] [1 2]))

(->> slopes
     (map count-trees)
     (reduce *))
