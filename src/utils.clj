(ns utils
  (:gen-class))

(defn find-first
  [f coll]
  (->> coll
       (filter f)
       first))
