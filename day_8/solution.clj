(ns day_8.solution
  (:require [clojure.string :as s]
            [clojure.set :as ss]
            [utils :as u]))

(def input
  (-> "day_8/input.txt"
      slurp
      s/split-lines))

(def instructions
  (map #(let [[type value] (s/split % #" ")]
          [(keyword type) (Integer. value)])
       input))

(defmulti execute
  (fn [instruction _]
    (first instruction)))

(defmethod execute :jmp
  [[_ value] {:keys [index acc]}]
  {:index (+ index value)
   :acc acc})

(defmethod execute :acc
  [[_ value] {:keys [index acc]}]
  {:index (inc index)
   :acc (+ acc value)})

(defmethod execute :nop
  [[_ _] {:keys [index acc]}]
  {:index (inc index)
   :acc acc})

(defn apply-instruction [[instruction globals visited-indeces my-instructions]]
  (let [{:keys [index] :as new-globals} (execute instruction globals)
        next-instruction (nth my-instructions index)
        new-visited-indeces (conj visited-indeces (:index globals))]
    [next-instruction new-globals new-visited-indeces my-instructions]))

^{:meta "part1"}
(defn instruction-path [my-instructions]
  (take-while
    (fn [[_ {:keys [index acc]} visited-indeces _]]
      (not (contains? visited-indeces index)))
    (iterate apply-instruction [(first my-instructions)
                                {:index 0 :acc 0}
                                #{}
                                my-instructions])))

;---

(def visited-indices
  (-> instructions
      instruction-path
      last
      (nth 2)))

(def candidate-indices
  (filter #(let [[type _] (nth instructions %)]
             (contains? #{:jmp :nop} type))
          visited-indices))

(def candidate-instructions
  (map #(let [[type value] (nth instructions %)
              new-instruction [(case type
                                 :jmp :nop
                                 :nop :jmp) value]]
          (assoc (vec instructions) % new-instruction))
        candidate-indices))

(defn instruction-path* [my-instructions]
  (take-while
    (fn [[_ {:keys [index acc]} visited-indeces _]]
      (and (< index (dec (count instructions)))
           (not (contains? visited-indeces index))))
    (iterate apply-instruction [(first my-instructions)
                                {:index 0 :acc 0}
                                #{}
                                my-instructions])))

(->> candidate-instructions
     (map-indexed
       (fn [index my-instructions]
         (let [path (instruction-path* my-instructions)
               last-index-on-path (-> path
                                      last
                                      (nth 1)
                                      :index)]
           [last-index-on-path index])))
     (u/find-first (fn [[path-index instructions-number]]
                     (= path-index 606))) ;; 606 is our max index found
     time) ; => 29

^{:meta "part2"} ;; I'm not very happy with this bruteforce solution
(dec (nth (last (instruction-path* (nth candidate-instructions 29))) 1))
